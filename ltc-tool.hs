{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Version ( showVersion )
import Ltc.Store
import Network.BSD ( getHostName )
import Network.RedisServer ( serve )
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
    , Redis { dir = "redis-store" &= typDir }
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
            -- when (null d) $ fail "Given directory cannot be empty"
            _ <- printf "Running Redis server with %s\n" d
            -- FIXME Implement clean termination for server.
            hostname <- getHostName
            store <- open (OpenParameters { location       = d
                                          , useCompression = False
                                          , nodeName       = (BL.pack hostname) })
            serve store `CE.finally` close store
