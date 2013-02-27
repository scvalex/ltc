{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, FlexibleContexts #-}

module Main where

import Control.Applicative ( (<$>) )
import Control.Concurrent.MVar ( newEmptyMVar, putMVar, takeMVar )
import Data.Foldable ( foldlM )
import Data.Map ( Map )
import Data.Traversable ( forM )
import Data.Version ( showVersion )
import Language.Sexp ( Sexp(..), Sexpable(..), printHum )
import Ltc.Store
import Ltc.Store.Diff ( Diff, Diffable(..) )
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
    , DiffPack { dir = def &= typDir &= argPos 1 }
      &= help "dump a store to a single file"
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
            dp <- foldlM (addKeyHistory store) (Diffs M.empty) ks
            BL.putStrLn (printHum (toSexp dp))
            close store
        Redis d -> do
            -- when (null d) $ fail "Given directory cannot be empty"
            _ <- printf "Running Redis server with %s\n" d
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
    addKeyHistory store (Diffs m) key = do
        mkh <- do
            mkhInt <- getKeyHistory store key
            case mkhInt of
                 Just (tip :: Value (Single Integer), diffs) ->
                     return (Just (KeyHistory (tip, diffs)))
                 Nothing -> do
                     mkhIntSet <- getKeyHistory store key
                     case mkhIntSet of
                         Just (tip :: Value (Collection Integer), diffs) ->
                             return (Just (KeyHistory (tip, diffs)))
                         Nothing -> do
                             mkhString <- getKeyHistory store key
                             case mkhString of
                                 Just (tip :: Value (Single BL.ByteString), diffs) ->
                                     return (Just (KeyHistory (tip, diffs)))
                                 Nothing -> do
                                     mkhStringSet <- getKeyHistory store key
                                     case mkhStringSet of
                                         Just (tip :: Value (Collection BL.ByteString), diffs) ->
                                             return (Just (KeyHistory (tip, diffs)))
                                         Nothing ->
                                             return Nothing
        case mkh of
            Nothing -> do
                fail (printf "unknown type for key %s" (show key))
            Just kh ->
                return (Diffs (M.insert key kh m))

    getKeyHistory :: forall a s. (Store s, ValueString (Value a), Diffable a)
                  => s -> Key -> IO (Maybe (Value a, [Diff a]))
    getKeyHistory store key = do
        CE.handle (\(_ :: CE.SomeException) -> return Nothing) $ do
            Just (tip :: Value a, _) <- getLatest store key
            vsns <- storeUnJust =<< keyVersions store key
            -- @vsns@ contains at least the tip.
            vs <- forM (tail vsns) (\vsn -> storeUnJust =<< get store key vsn)
            let (_, diffs) = foldl (\(v, ds) v' -> (v', reverseDiff (diffFromTo v v') : ds))
                                   (tip, [])
                                   vs
            return (Just (tip, diffs))

    storeUnJust (Just a) = return a
    storeUnJust Nothing  = fail "could not find expected value in store"

data KeyHistory = forall a. (Sexpable (Diff a), Sexpable (Value a))
                  => KeyHistory (Value a, [Diff a])

instance Sexpable KeyHistory where
    toSexp (KeyHistory (tip, history)) = List ["KeyHistory", toSexp tip, toSexp history]
    fromSexp _ = undefined

data Diffs = Diffs (Map Key KeyHistory)

instance Sexpable Diffs where
    toSexp (Diffs ds) = List ["Diffs", toSexp ds]
    fromSexp _ = fail "fromSexp Diffs not implemented"
