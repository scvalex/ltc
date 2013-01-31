{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Applicative ( (<$>) )
import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable ( foldlM )
import qualified Data.Set as S
import Data.Version ( showVersion )
import Ltc.Store
import Network.BSD ( getHostName )
import Network.RedisServer ( serve )
import System.Console.CmdArgs
import Paths_ltc ( version )
import Text.Printf ( printf )

data Modes = Fsck { dir :: FilePath }
           | Info { dir :: FilePath }
           | Redis { dir :: FilePath }
           deriving ( Show, Data, Typeable )

ltcModes :: [Modes]
ltcModes =
    [ Fsck { dir = def &= typDir &= argPos 1 }
      &= help "check the integrity of a store"
    , Info { dir = def &= typDir &= argPos 1 }
      &= help "list information about a store"
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
        Info d -> do
            hostname <- getHostName
            store <- open (OpenParameters { location       = d
                                          , useCompression = False
                                          , nodeName       = (BL.pack hostname) })
            _ <- printf "LTc store: %s (format %s-%d)\n" d (storeFormat store) (storeVersion store)
            _ <- printf "  node     : %s\n" hostname
            ks <- keys store
            _ <- printf "  keys     : %d\n" (S.size ks)
            vn <- foldlM (\n k -> maybe n ((n+) . length) <$> keyVersions store k) 0 ks
            _ <- printf "  values   : %d\n" vn
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
