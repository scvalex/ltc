module Test.Common (
        cleanEnvironment, cleanEnvironmentP
    ) where

import Control.Monad ( when )
import System.Directory ( doesDirectoryExist, removeDirectoryRecursive
                        , doesFileExist, removeFile )
import Test.HUnit
import Test.QuickCheck.Monadic as QCM

-- | Test an assertion in a clean environment and cleanup afterwards.
cleanEnvironment :: [FilePath] -> Assertion -> Assertion
cleanEnvironment files ass = do
    mapM_ rmrf files
    ass

cleanEnvironmentP :: [FilePath] -> PropertyM IO a -> PropertyM IO a
cleanEnvironmentP files prop = do
    run $ mapM_ rmrf files
    prop

-- | Remove a file or a directory recursively.
rmrf :: FilePath -> IO ()
rmrf fp = do
    dde <- doesDirectoryExist fp
    when dde $ removeDirectoryRecursive fp
    dfe <- doesFileExist fp
    when dfe $ removeFile fp
