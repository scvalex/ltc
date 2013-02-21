{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, FlexibleContexts #-}

module Main where

import Control.Applicative ( (<$>) )
import Control.Concurrent.MVar ( newEmptyMVar, putMVar, takeMVar )
import Data.Foldable ( foldlM )
import Data.Map ( Map )
import Data.Version ( showVersion )
import Language.Sexp ( Sexp(..), Sexpable(..), printHum )
import Ltc.Store
import Ltc.Store.Diff ( Diff )
import Network.BSD ( getHostName )
import Network.RedisServer ( serve )
import Paths_ltc ( version )
import System.Console.CmdArgs
import System.Posix.Signals ( Handler(..), installHandler, sigINT )
import Text.Printf ( printf )
import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Data.Set as S

data Modes = Fsck { dir :: FilePath }
           | Info { dir :: FilePath }
           | DiffPack { dir :: FilePath }
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
            store <- open (openParameters d hostname)
            close store
        Info d -> do
            hostname <- getHostName
            store <- open (openParameters d hostname)
            _ <- printf "LTc store: %s (format %s-%d)\n" d (storeFormat store) (storeVersion store)
            _ <- printf "  node     : %s\n" hostname
            ks <- keys store
            _ <- printf "  keys     : %d\n" (S.size ks)
            vn <- foldlM (\n k -> maybe n ((n+) . length) <$> keyVersions store k) 0 ks
            _ <- printf "  values   : %d\n" vn
            close store
        DiffPack d -> do
            hostname <- getHostName
            store <- open (openParameters d hostname)
            ks <- keys store
            let dp = Diffs M.empty
            BL.putStrLn (printHum (toSexp dp))
            close store
        Redis d -> do
            -- when (null d) $ fail "Given directory cannot be empty"
            _ <- printf "Running Redis server with %s\n" d
            -- FIXME Implement clean termination for server.
            hostname <- getHostName
            store <- open (openParameters d hostname)
            shutdown <- serve store
            done <- newEmptyMVar
            _ <- installHandler sigINT (Catch $ putMVar done ()) Nothing
            takeMVar done `CE.finally` (do
                _ <- printf "Shutting down... "
                shutdown
                close store
                _ <- printf "done\n"
                return ())
  where
    openParameters d hostname =
        OpenParameters { location       = d
                       , useCompression = False
                       , nodeName       = (BL.pack hostname) }

data DiffPack = forall a. (Sexpable (Diff a), Sexpable (Value a)) => Diffs (Map Key (Value a, [Diff a]))

instance Sexpable DiffPack where
    toSexp (Diffs ds) = List ["DiffPack", toSexp ds]
    fromSexp (List ["DiffPack", _]) = fail "fromSexp DiffPack not implemented"
