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
import qualified Data.VectorClock as VC
import System.Directory

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
    opened <- CE.handle (\(_ :: CE.SomeException) -> return False) $ do
        _ <- open (op { nodeName = "other-test" })
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

instance Arbitrary Commands where
    arbitrary = sized $ \n -> do
        let kn = ceiling (sqrt (fromIntegral n :: Double)) :: Int
        keys <- replicateM kn arbitrary
        Commands <$> replicateM n (makeCommand keys)

-- | For any store @store@, and any key @k@, the value of @getLatest
-- store k@ should either be 'Nothing', if @key@ was never set in this
-- store, or @Just (v, vsn)@, where @v@ is parameter to the most
-- recent @set store k v@, and @vsn@ is the returned value of the same
-- command.
propSetGetLatest :: Commands -> Property
propSetGetLatest cmds = monadicIO $ do
    cleanEnvironmentP ["test-store"] $ do
        store <- run $ open (OpenParameters { location       = "test-store"
                                            , useCompression = False
                                            , nodeName       = "test" })
        _ <- foldlM (runCmd store) M.empty (unCommands cmds)
        run $ close store
  where
    runCmd store kvs cmd = do
        case cmd of
            GetLatest key -> do
                res <- run $ getLatest store key
                QCM.assert (res == M.lookup key kvs)
                return kvs
            Set key value -> do
                vsn <- run $ set store key value
                case M.lookup key kvs of
                    Nothing          -> return ()
                    Just (_, vsnOld) -> QCM.assert (vsnOld `VC.causes` vsn)
                return (M.insert key (value, vsn) kvs)

-- | For a non-forgetful $store$, /all/ values @v@ inserted by @set
-- store k v$ should still be available to @get store k vsn@, where
-- @vsn@ is the value returned by the corresponding @set@.
propFullHistory :: Commands -> Property
propFullHistory cmds = monadicIO $ do
    cleanEnvironmentP ["test-store"] $ do
        store <- run $ open (OpenParameters { location       = "test-store"
                                            , useCompression = False
                                            , nodeName       = "test" })
        _ <- foldlM (runCmd store) (M.empty, []) (unCommands cmds)
        run $ close store
  where
    runCmd store (kvsns, kvs) cmd = do
        case cmd of
            GetLatest key -> do
                mvsns <- run $ keyVersions store key
                QCM.assert (mvsns == M.lookup key kvsns)
                case mvsns of
                    Nothing -> return ()
                    Just vsns ->
                        -- Theoretically, getting the versions above,
                        -- and iterating through them below is a race.
                        -- Practically, meh.
                        forM_ vsns $ \vsn -> do
                            res <- run $ get store key vsn
                            QCM.assert (res == ((\(_, _, v) -> v)
                                                <$> find (\(k, vsn', _) ->
                                                           k == key && vsn == vsn') kvs))
                return (kvsns, kvs)
            Set key value -> do
                vsn <- run $ set store key value
                let kvsns' = case M.lookup key kvsns of
                        Nothing   -> M.insert key [vsn] kvsns
                        Just vsns -> M.insert key (vsn:vsns) kvsns
                return (kvsns', (key, vsn, value):kvs)

--------------------------------
-- Helpers
--------------------------------

-- | Remove a file or a directory recursively.
rmrf :: FilePath -> IO ()
rmrf fp = do
    dde <- doesDirectoryExist fp
    when dde $ removeDirectoryRecursive fp
    dfe <- doesFileExist fp
    when dfe $ removeFile fp

-- | Test an assertion in a clean environment and cleanup afterwards.
cleanEnvironment :: [FilePath] -> Assertion -> Assertion
cleanEnvironment files ass = do
    mapM_ rmrf files
    ass

cleanEnvironmentP :: [FilePath] -> PropertyM IO a -> PropertyM IO a
cleanEnvironmentP files prop = do
    run $ mapM_ rmrf files
    prop

-- | Given a set of keys, generate an arbitrary command.  @Get@s and
-- @Set@s have equal probability.
makeCommand :: [Key] -> Gen Command
makeCommand keys = do
    n <- choose (1, 2 :: Int)
    let key = elements keys
    case n of
        1 -> GetLatest <$> key
        2 -> Set <$> key <*> arbitrary
        _ -> fail "unknown case in 'Arbitrary Command'"
