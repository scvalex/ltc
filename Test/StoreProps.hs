{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Ltc.Store

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BL
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
       ] mempty

--------------------------------
-- Unit tests
--------------------------------

testOpen :: Assertion
testOpen = cleanEnvironment ["test-store"] $ do
    store <- open (OpenParameters { location       = "test-store"
                                  , useCompression = False
                                  , nodeName       = "test" })
    close store

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
      where
        makeCommand keys = do
            n <- choose (1, 2 :: Int)
            let key = elements keys
            case n of
                1 -> GetLatest <$> key
                2 -> Set <$> key <*> arbitrary
                _ -> fail "unknown case in 'Arbitrary Command'"

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
