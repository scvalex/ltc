{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ltc.Store

import Control.Exception ( finally )
import Control.Monad
import Data.Monoid
import qualified Data.VectorClock as VC
import System.Directory

import Test.Framework
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
import Test.HUnit
-- import Test.QuickCheck

--FIXME Add QC test Simple test-suite

main :: IO ()
main = defaultMainWithOpts
       [ testCase "open" testOpen
       , testCase "simpleSetGet" testSimpleSetGet
--       , testProperty "idemParseShowStatement" prop_IdemParseShowStatement
       ] opts
  where
    opts = mempty {
             ropt_test_options =
                 Just (mempty
                       { topt_maximum_generated_tests            = Just 500
                       , topt_maximum_unsuitable_generated_tests = Just 5000
                       })}

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
    _ <- set store "foo" "baz"
    res4 <- getLatest store "foo"
    res4 @?= Just ("baz", VC.fromList [("test", 2)])
    close store

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
    -- FIXME Re-enable cleanup after tests.
    ass `finally` return () --mapM_ rmrf files
