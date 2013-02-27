module Common (
        cleanEnvironment, cleanEnvironmentP, testParameters
    ) where

import Control.Monad ( when )
import Data.ByteString.Lazy.Char8 ( pack )
import Ltc.Store
import Network.BSD ( getHostName )
import System.Directory ( doesDirectoryExist, removeDirectoryRecursive
                        , doesFileExist, removeFile )
import System.IO.Unsafe ( unsafePerformIO )
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
                                , nodeName       = pack hostname }
  where
    hostname = unsafePerformIO getHostName