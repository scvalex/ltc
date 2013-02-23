{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Ltc.Store

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Foldable ( foldlM )
import Data.List ( find )
import Data.Monoid
import Network.BSD ( getHostName )
import Test.Common ( cleanEnvironment, cleanEnvironmentP, testParameters )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic as QCM
import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.VectorClock as VC

main :: IO ()
main = defaultMainWithOpts
       [ testCase "open" testOpen
       , testCase "simpleSetGet" testSimpleSetGet
       , testCase "simpleHistory" testSimpleHistory
       , testCase "simpleFieldType" testSimpleFieldType
       , testProperty "setGetLatest" propSetGetLatest
       , testProperty "keysPresent" propKeysPresent
       , testProperty "fullHistory" propFullHistory
       ] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 10000000) }) }

--------------------------------
-- Unit tests
--------------------------------

testOpen :: Assertion
testOpen = cleanEnvironment ["test-store"] $ do
    hostname <- BL.pack <$> getHostName
    store <- open testParameters
    close store
    store' <- open testParameters
    close store'
    opened <- CE.handle (\(exn :: NodeNameMismatchError) ->
                          return (not (requestedName exn == "other"
                                       && storeName exn == hostname))) $ do
        _ <- open (testParameters { nodeName = "other" })
        return True
    when opened $ assertFailure "re-opened store with different node name"

testSimpleSetGet :: Assertion
testSimpleSetGet = cleanEnvironment ["test-store"] $ do
    hostname <- BL.pack <$> getHostName
    store <- open testParameters
    _ <- set store "foo" (vs "bar")
    res1 <- getLatest store "foo"
    res1 @?= Just (vs "bar", VC.fromList [(hostname, 1)])
    res2 <- getLatest store "bar"
    res2 @?= (Nothing :: Maybe (Value (Single ByteString), Version))
    _ <- set store "bar" (vs "baz")
    res3 <- getLatest store "bar"
    res3 @?= Just (vs "baz", VC.fromList [(hostname, 1)])
    _ <- set store "foo" (vs "boom")
    res4 <- getLatest store "foo"
    res4 @?= Just (vs "boom", VC.fromList [(hostname, 2)])
    ks <- keys store
    ks @?= S.fromList ["foo", "bar"]
    close store

testSimpleHistory :: Assertion
testSimpleHistory = cleanEnvironment ["test-store"] $ do
    store <- open testParameters
    v1 <- set store "foo" (vs "bar")
    res1 <- getLatest store "foo"
    res1 @?= Just (vs "bar", v1)
    v2 <- set store "foo" (vs "baz")
    res2 <- getLatest store "foo"
    res2 @?= Just (vs "baz", v2)
    res3 <- get store "foo" v1
    res3 @?= Just (vs "bar")
    close store

testSimpleFieldType :: Assertion
testSimpleFieldType = cleanEnvironment ["test-store"] $ do
    store <- open testParameters
    v1 <- set store "foo" (VaInt 23)
    res1 <- getLatest store "foo"
    res1 @?= Just (VaInt 23, v1)
    done <- CE.handle (\(exn :: TypeMismatchError) ->
                          return (not (expectedType exn == SingleInteger
                                       && foundType exn == SingleString))) $ do
        _ <- set store "foo" (vs "bar")
        return True
    when done $ assertFailure "set a key with a different type"
    close store

--------------------------------
-- QuickCheck
--------------------------------

instance Arbitrary ByteString where
    arbitrary = sized $ \n -> do
        BL.pack <$> sequence [ choose (' ', '~') | _ <- [1..n] ]

data Command = GetLatest Key | Set Key (Value (Single ByteString))
             deriving ( Show )

newtype Commands = Commands { unCommands :: [Command] }
                 deriving ( Show )

instance Arbitrary (Value (Single ByteString)) where
    arbitrary = VaString <$> arbitrary

instance Arbitrary Key where
    arbitrary = Key <$> arbitrary

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
        (_ :: Maybe (Value (Single ByteString), Version)) <- run $ getLatest store key
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
        store <- run $ open testParameters
        _ <- prop store (unCommands cmds)
        run $ close store

vs :: String -> Value (Single ByteString)
vs = VaString . BL.pack
