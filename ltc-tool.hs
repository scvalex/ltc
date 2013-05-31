{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Applicative ( Applicative, (<$>) )
import Control.Concurrent.MVar ( newEmptyMVar, putMVar, takeMVar )
import Control.Monad ( forM_ )
import Control.Monad.Trans.Class ( lift )
import Data.Char ( isAlphaNum )
import Data.List ( nub )
import Data.Version ( showVersion )
import Language.Sexp ( Sexpable(..), printHum, printMach, parseExn, parse )
import Ltc.Store ( Store(..), withStore
                 , Key(..), Value(..), Single
                 , keyVersionsExn, getExn )
import Ltc.Store.Simple ( Simple, OpenParameters(..), createIfMissing )
import Ltc.Store.VersionControl ( DiffPack, getDiffPack
                                , insertChangesInto )
import qualified Ltc.Monkey as M
import Network.BSD ( getHostName )
import Paths_ltc ( version )
import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import qualified Network.Interface.UDP as U
import qualified Network.NodeServer as N
import qualified Network.StatusServer as S
import qualified Network.NodeProtocol as P
import qualified Network.RedisServer as R
import System.Console.CmdArgs
import System.Console.Haskeline ( InputT, runInputT
                                , Settings(..), defaultSettings
                                , getInputLine, outputStrLn
                                , Interrupt(..), withInterrupt )
import System.Directory ( getHomeDirectory )
import System.FilePath ( (</>) )
import System.IO ( stdout )
import System.Log.Logger ( Priority(..), setLevel
                         , updateGlobalLogger, rootLoggerName, setHandlers )
import System.Log.Handler.Simple ( verboseStreamHandler )
import System.Posix.Signals ( Handler(..), installHandler, sigINT )
import System.Random ( randomRIO )
import Text.Printf ( printf )

----------------------
-- cmdargs configuration
----------------------

data Modes = Fsck { dir :: FilePath }
           | Info { dir :: FilePath, listKeys :: Bool }
           | Export { dir :: FilePath, file :: FilePath }
           | Import { dir :: FilePath, file :: FilePath }
           | Populate { dir :: FilePath, count :: Int }
           | Redis { dir :: FilePath }
           | Node { storeDir :: Maybe FilePath, nodeIndex :: Int }
           | WireClient { host :: String, port :: Int }
           deriving ( Show, Data, Typeable )

ltcModes :: [Modes]
ltcModes =
    [ Fsck { dir = def &= typDir &= argPos 0 }
      &= help "check the integrity of a store"
    , Info { dir = def &= typDir &= argPos 0
           , listKeys = False &= help "list the keys in a store" }
      &= help "list information about a store"
    , Export { dir = def &= typDir &= argPos 0
             , file = "changes.sexp" &= typFile }
      &= help "export all changes to a file"
    , Import { dir = def &= typDir &= argPos 0
             , file = "changes.sexp" &= typFile }
      &= help "import changes from a file"
    , Populate { dir = def &= typDir &= argPos 0
               , count = 100 &= help "around how many keys to insert" }
      &= help "populate a store with random values"
    , Redis { dir = "store" &= typDir }
      &= help "run a store with a Redis interface"
    , Node { storeDir = def &= typDir
           , nodeIndex = 0 &= help "what is the index of this node on this machine" }
      &= help "run a store with an LTc node interface"
    , WireClient { host = "localhost" &= typ "HOST"
                 , port = N.nodePort &= typ "PORT" }
      &= help "connect a low-level client to an LTc node"
    ]
    &= program "ltc"
    &= summary (printf "ltc v%s - LTc utility" (showVersion version))

----------------------
-- Main
----------------------

main :: IO ()
main = do
    handler <- verboseStreamHandler stdout DEBUG
    updateGlobalLogger rootLoggerName (setHandlers [handler] . setLevel DEBUG)
    opts <- cmdArgs $ modes ltcModes
    case opts of
        Fsck d -> do
            _ <- printf "Checking %s...\n" d
            hostname <- getHostName
            withStore ((openParameters d hostname 0 False))
                (\_ -> return ())
        Info d lk -> do
            hostname <- getHostName
            withStore (openParameters d hostname 0 False)
                (doInfo d lk hostname)
        Export d fo -> do
            hostname <- getHostName
            withStore (openParameters d hostname 0 False) $ \store -> do
                dp <- getDiffPack store
                BL.writeFile fo (printHum (toSexp dp))
        Import d fi -> do
            hostname <- getHostName
            withStore (openParameters d hostname 0 False) $ \store -> do
                (Just dp :: Maybe DiffPack) <- fromSexp . head . parseExn <$> BL.readFile fi
                conflicts <- insertChangesInto store dp
                case conflicts of
                    [] -> return ()
                    _  -> printf "%d conflicts\n" (length conflicts)
        Populate d cnt -> do
            _ <- printf "Populating %s\n" d
            hostname <- getHostName
            withStore (openParameters d hostname 0 True)
                (doPopulate cnt)
        Redis d -> do
            -- when (null d) $ fail "Given directory cannot be empty"
            _ <- printf "Running Redis server with %s\n" d
            hostname <- getHostName
            store <- open (openParameters d hostname 0 True)
            shutdown <- R.serve store
            shutdownOnInt store [shutdown]
        Node mStoreDir idx -> do
            let myStoreDir = maybe (printf "node-store-%d" idx) id mStoreDir
            _ <- printf "Running Node on %d with %s\n" (N.nodePort + idx) myStoreDir
            hostname <- getHostName
            store <- open (openParameters myStoreDir hostname idx True)
            node <- N.serveFromLocation (U.NetworkLocation { U.host = hostname
                                                           , U.port = N.nodePort + idx })
                                        store
            -- FIXME Remove hacky "connect to next node in the ring"
            N.addNeighbour node (U.NetworkLocation { U.host = hostname
                                                   , U.port = N.nodePort + idx + 1 })
            status <- S.serveWithPort (S.statusPort + idx) store
            monkey <- M.start store
            shutdownOnInt store [N.shutdown node, S.shutdown status, M.shutdown monkey]
        WireClient h p -> do
            _ <- printf "Connecting wire client to %s:%d\n" h p
            hostname <- getHostName
            store <- open (openParameters "wire-client-store" hostname 0 True)
            node <- N.serveFromLocation (U.NetworkLocation { U.host = hostname
                                                           , U.port = N.nodePort + 11 })
                                        store
            conn <- N.connect node (U.NetworkLocation { U.host = h
                                                      , U.port = p })
            homeDir <- getHomeDirectory
            CE.handle (\Interrupt -> return ())
                      (flip runInputT (withInterrupt (repl node conn)) $
                                      defaultSettings {
                                          historyFile = Just (homeDir </> ".ltc_history") })
            shutdownNow [N.closeConnection conn, N.shutdown node, close store]
  where
    openParameters :: String -> String -> Int -> Bool -> OpenParameters Simple
    openParameters d hostname idx shouldCreate =
        OpenParameters { location        = d
                       , useCompression  = False
                       , nodeName        = (BL.pack (printf "%s-%d" hostname idx))
                       , createIfMissing = shouldCreate
                       , forceOpen       = not shouldCreate
                       }

    -- | Run all shutdown actions in sequence.
    shutdownNow :: [IO ()] -> IO ()
    shutdownNow shutdowns = do
        _ <- printf "Shutting down... "
        sequence_ shutdowns
        _ <- printf "done\n"
        return ()

    shutdownOnInt :: (Store s) => s -> [IO ()] -> IO ()
    shutdownOnInt store shutdowns = do
        done <- newEmptyMVar
        _ <- installHandler sigINT (Catch $ putMVar done ()) Nothing
        takeMVar done `CE.finally` shutdownNow (shutdowns ++ [close store])

    doInfo :: (Store s) => FilePath -> Bool -> String -> s -> IO ()
    doInfo d lk hostname store = do
        _ <- printf "LTc store: %s (format %s-%d)\n" d (storeFormat store) (storeVersion store)
        _ <- printf "  node     : %s\n" hostname
        ks <- keys store
        if lk
            then do
                _ <- printf "  keys     :\n"
                forM_ (S.toList ks) $ \key -> do
                    vsns <- keyVersionsExn store key
                    _ <- printf "    %s:\n" (show key)
                    forM_ vsns $ \vsn -> do
                        _ <- printf "      %s\n" (BL.unpack (printMach (toSexp vsn)))
                        CE.handle (\(_ :: CE.SomeException) -> do
                                        CE.handle (\(_ :: CE.SomeException) -> do
                                                        putStrLn "        non string value") (do
                                            (v :: Value (Single BL.ByteString)) <- getExn store key vsn
                                            _ <- printf "        %s\n" (BL.unpack (printMach (toSexp v)))
                                            return ())) $ do
                            (v :: Value (Single Integer)) <- getExn store key vsn
                            _ <- printf "        %s\n" (BL.unpack (printMach (toSexp v)))
                            return ()
                return ()
            else do
                _ <- printf "  keys     : %d\n" (S.size ks)
                return ()

    -- FIXME Support re-populating.
    doPopulate :: (Store s) => Int -> s -> IO ()
    doPopulate cnt store = do
        ks <- mapM (\_ -> Key <$> someWord) [1..cnt]
        forM_ (nub ks) $ \key -> do
            t <- randomRIO (1, 2 :: Int)
            case t of
                1 -> forM_ [1..5 :: Int] $ \_ -> do
                    v <- VaInt <$> randomRIO (0, 100)
                    _ <- set store key v
                    return ()
                2 -> forM_ [1..5 :: Int] $ \_ -> do
                    v <- VaString <$> someWord
                    _ <- set store key v
                    return ()
                _ -> do
                    return ()
      where
        someWord = BL.pack . (dict !!) <$> randomRIO (0 :: Int, length dict - 1)

        dict = words (filter (\c -> c == ' ' || isAlphaNum c) $
               "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

    -- | Read a 'NodeMessage' from an S-Expression, encode it with cereal, and send it to
    -- the remote node.
    repl :: N.Node U.UdpInterface -> N.Connection U.UdpInterface -> InputT IO ()
    repl node conn = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> do
                return ()
            Just input -> do
                case readSexpString input :: Maybe P.NodeMessage of
                    Nothing -> do
                        outputStrLn "Borked input"
                        repl node conn
                    Just msg -> do
                        lift $ N.sendMessage node conn msg
                        outputStrLn (show msg)
                        repl node conn

    -- FIXME Move this (and a BS version) to sexp
    -- | Read a single value from an S-Expression in a 'String'.
    readSexpString :: (Applicative m, Monad m, Sexpable a) => String -> m a
    readSexpString str = do
        case parse (BL.pack str) of
            Left (err, _) -> do
                fail err
            Right [s] -> do
                fromSexp s
            Right _ -> do
                fail "expecting just one S-Expression"
