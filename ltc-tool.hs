{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Version ( showVersion )
import Ltc.Store
import Network.BSD ( getHostName )
import System.Console.CmdArgs
import Paths_ltc ( version )
import Text.Printf ( printf )

data Modes = Fsck { dir :: FilePath }
           | Redis { dir :: FilePath }
           deriving ( Show, Data, Typeable )

ltcModes :: [Modes]
ltcModes =
    [ Fsck { dir = def &= typDir &= argPos 1 }
      &= help "check the integrity of a store"
    , Redis { dir = def &= typDir }
      &= help "run a store with a Redis interface"
    ]
    &= program "ltc"
    &= summary (printf "ltc v%s - LTc utility" (showVersion version))

main :: IO ()
main = do
    opts <- cmdArgs $ modes ltcModes
    case opts of
        Fsck d -> do
            _ <- printf "Checking %s...\n" d
            hostname <- getHostName
            store <- open (OpenParameters { location       = d
                                          , useCompression = False
                                          , nodeName       = (BL.pack hostname) })
            close store
        Redis d -> do
            _ <- printf "Running Redis server with %s\n" d
            return ()
