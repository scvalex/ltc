module Test.Common (
        cleanEnvironment, cleanEnvironmentP, testParameters
    ) where

import Ltc.Store

import Control.Monad ( when )
import System.Directory ( doesDirectoryExist, removeDirectoryRecursive
                        , doesFileExist, removeFile )
import Test.HUnit
import Test.QuickCheck.Monadic as QCM

----------------------
-- Cleanup
----------------------

-- | Test an assertion in a clean environment and cleanup afterwards.
cleanEnvironment :: [FilePath] -> Assertion -> Assertion
cleanEnvironment files ass = do
    mapM_ rmrf files
    ass

cleanEnvironmentP :: [FilePath] -> PropertyM IO a -> PropertyM IO a
cleanEnvironmentP files prop = do
    run $ mapM_ rmrf files
    prop

----------------------
-- Helpers
----------------------

-- | Remove a file or a directory recursively.
rmrf :: FilePath -> IO ()
rmrf fp = do
    dde <- doesDirectoryExist fp
    when dde $ removeDirectoryRecursive fp
    dfe <- doesFileExist fp
    when dfe $ removeFile fp

----------------------
-- Repeated values
----------------------

testParameters :: OpenParameters Simple
testParameters = OpenParameters { location       = "test-store"
                                , useCompression = False
                                , nodeName       = "test" }
