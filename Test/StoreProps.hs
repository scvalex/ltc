{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Ltc.Store

import Control.Applicative
import qualified Control.Exception as CE
import Control.Monad
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List ( find )
import Data.Foldable ( foldlM )
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import qualified Data.VectorClock as VC

import Test.Common ( cleanEnvironment, cleanEnvironmentP )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic as QCM

main :: IO ()
main = defaultMainWithOpts
       [ testCase "open" testOpen
       , testCase "simpleSetGet" testSimpleSetGet
       , testCase "simpleHistory" testSimpleHistory
       , testProperty "setGetLatest" propSetGetLatest
       , testProperty "keysPresent" propKeysPresent
       , testProperty "fullHistory" propFullHistory
       ] mempty

--------------------------------
-- Unit tests
--------------------------------

testOpen :: Assertion
testOpen = cleanEnvironment ["test-store"] $ do
    let op = OpenParameters { location       = "test-store"
                            , useCompression = False
                            , nodeName       = "test" }
    store <- open op
    close store
    store' <- open op
    close store'
    opened <- CE.handle (\(exn :: NodeNameMismatchError) ->
                          return (not (requestedName exn == "other"
                                       && storeName exn == "test"))) $ do
        _ <- open (op { nodeName = "other" })
        return True
    when opened $ assertFailure "re-opened store with different node name"

testSimpleSetGet :: Assertion
testSimpleSetGet = cleanEnvironment ["test-store"] $ do
    store <- open (OpenParameters { location       = "test-store"
                                  , useCompression = False
                                  , nodeName       = "test" })
    _ <- set store "foo" "bar"
    res1 <- getLatest store "foo"
    res1 @?= Just ("bar", VC.fromList [("test", 1)])
    res2 <- getLatest store "bar"
    res2 @?= Nothing
    _ <- set store "bar" "baz"
    res3 <- getLatest store "bar"
    res3 @?= Just ("baz", VC.fromList [("test", 1)])
    _ <- set store "foo" "boom"
    res4 <- getLatest store "foo"
    res4 @?= Just ("boom", VC.fromList [("test", 2)])
    ks <- keys store
    ks @?= S.fromList ["foo", "bar"]
    close store

testSimpleHistory :: Assertion
testSimpleHistory = cleanEnvironment ["test-store"] $ do
    store <- open (OpenParameters { location       = "test-store"
                                  , useCompression = False
                                  , nodeName       = "test" })
    v1 <- set store "foo" "bar"
    res1 <- getLatest store "foo"
    res1 @?= Just ("bar", v1)
    v2 <- set store "foo" "baz"
    res2 <- getLatest store "foo"
    res2 @?= Just ("baz", v2)
    res3 <- get store "foo" v1
    res3 @?= Just "bar"

--------------------------------
-- QuickCheck
--------------------------------

data Command = GetLatest Key | Set Key Value
             deriving ( Show )

newtype Commands = Commands { unCommands :: [Command] }
                 deriving ( Show )

instance Arbitrary ByteString where
    arbitrary = sized $ \n -> do
        BL.pack <$> sequence [ choose (' ', '~') | _ <- [1..n] ]

instance Arbitrary Value where
    arbitrary = VaString <$> arbitrary

instance Arbitrary Commands where
    arbitrary = sized $ \n -> do
        let kn = ceiling (sqrt (fromIntegral n :: Double)) :: Int
        ks <- replicateM kn arbitrary
        Commands <$> replicateM n (makeCommand ks)

-- | For any store @store@, and any key @k@, the value of @getLatest
-- store k@ should either be 'Nothing', if @key@ was never set in this
-- store, or @Just (v, vsn)@, where @v@ is parameter to the most
-- recent @set store k v@, and @vsn@ is the returned value of the same
-- command.
propSetGetLatest :: Commands -> Property
propSetGetLatest = propWithCommands (\store cmds -> foldlM (runCmd store) M.empty cmds)
  where
    runCmd store kvs (GetLatest key) = do
        res <- run $ getLatest store key
        QCM.assert (res == M.lookup key kvs)
        return kvs
    runCmd store kvs (Set key value) = do
        vsn <- run $ set store key value
        case M.lookup key kvs of
            Nothing          -> return ()
            Just (_, vsnOld) -> QCM.assert (vsnOld `VC.causes` vsn)
        return (M.insert key (value, vsn) kvs)

-- | For a non-forgetful @store@, /all/ keys @k@ inserted by @set
-- store k v@ should be returned by subsequent @keys store@.
propKeysPresent :: Commands -> Property
propKeysPresent = propWithCommands (\store cmds -> foldlM (runCmd store) S.empty cmds)
  where
    runCmd store s (GetLatest key) = do
        _ <- run $ getLatest store key
        return s
    runCmd store s (Set key value) = do
        _ <- run $ set store key value
        let s' = S.insert key s
        ks <- run $ keys store
        QCM.assert (ks == s')
        return s'

-- | For a non-forgetful @store@, /all/ values @v@ inserted by @set
-- store k v$ should still be available to @get store k vsn@, where
-- @vsn@ is the value returned by the corresponding @set@.
propFullHistory :: Commands -> Property
propFullHistory = propWithCommands (\store cmds -> foldlM (runCmd store) (M.empty, []) cmds)
  where
    runCmd store (kvsns, kvs) (GetLatest key) = do
        mvsns <- run $ keyVersions store key
        QCM.assert (mvsns == M.lookup key kvsns)
        case mvsns of
            Nothing -> return ()
            Just vsns ->
                -- Theoretically, getting the versions above, and
                -- iterating through them below is a race.
                -- Practically, meh.
                forM_ vsns $ \vsn -> do
                    res <- run $ get store key vsn
                    QCM.assert (res == ((\(_, _, v) -> v)
                                        <$> find (\(k, vsn', _) ->
                                                   k == key && vsn == vsn') kvs))
        return (kvsns, kvs)
    runCmd store (kvsns, kvs) (Set key value) = do
        vsn <- run $ set store key value
        let kvsns' = case M.lookup key kvsns of
                Nothing   -> M.insert key [vsn] kvsns
                Just vsns -> M.insert key (vsn:vsns) kvsns
        return (kvsns', (key, vsn, value):kvs)

--------------------------------
-- Helpers
--------------------------------

-- | Given a set of keys, generate an arbitrary command.  @Get@s and
-- @Set@s have equal probability.
makeCommand :: [Key] -> Gen Command
makeCommand ks = do
    n <- choose (1, 2 :: Int)
    let key = elements ks
    case n of
        1 -> GetLatest <$> key
        2 -> Set <$> key <*> arbitrary
        _ -> fail "unknown case in 'Arbitrary Command'"

propWithCommands :: (Simple -> [Command] -> PropertyM IO a) -> Commands -> Property
propWithCommands prop cmds = monadicIO $ do
    cleanEnvironmentP ["test-store"] $ do
        store <- run $ open (OpenParameters { location       = "test-store"
                                            , useCompression = False
                                            , nodeName       = "test" })
        _ <- prop store (unCommands cmds)
        run $ close store
