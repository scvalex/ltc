{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ltc.Store

import Prelude hiding ( catch )

import Control.Exception
import Control.Monad
import Data.Monoid
import System.Directory

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

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
    store <- open (ConnectParameters { location = "test-store" })
    close store

testSimpleSetGet :: Assertion
testSimpleSetGet = cleanEnvironment ["test-store"] $ do
    store <- open (ConnectParameters { location = "test-store" })
    set store "foo" "bar"
    res <- getLatest store "foo"
    res @?= Just ("bar", 1)
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
    ass `finally` mapM_ rmrf files
