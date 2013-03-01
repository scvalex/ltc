{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Applicative ( (<$>) )
import Control.Concurrent.MVar ( newEmptyMVar, putMVar, takeMVar )
import Control.Monad ( forM_ )
import Data.Char ( isAlphaNum )
import Data.List ( nub )
import Data.Version ( showVersion )
import Language.Sexp ( Sexpable(..), printHum, printMach, parseExn )
import Ltc.Store
import Ltc.Store.Serialization ( DiffPack, getDiffPack )
import Ltc.Store.VersionControl ( insertChangesInto )
import Network.BSD ( getHostName )
import Network.RedisServer ( serve )
import Paths_ltc ( version )
import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import System.Console.CmdArgs
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
           deriving ( Show, Data, Typeable )

ltcModes :: [Modes]
ltcModes =
    [ Fsck { dir = def &= typDir &= argPos 1 }
      &= help "check the integrity of a store"
    , Info { dir = def &= typDir &= argPos 1
           , listKeys = False &= help "list the keys in a store" }
      &= help "list information about a store"
    , Export { dir = def &= typDir &= argPos 1
             , file = "changes.sexp" &= typFile }
      &= help "export all changes to a file"
    , Import { dir = def &= typDir &= argPos 1
             , file = "changes.sexp" &= typFile }
      &= help "import changes from a file"
    , Populate { dir = def &= typDir &= argPos 1
               , count = 100 &= help "around how many keys to insert" }
      &= help "populate a store with random values"
    , Redis { dir = "redis-store" &= typDir }
      &= help "run a store with a Redis interface"
    ]
    &= program "ltc"
    &= summary (printf "ltc v%s - LTc utility" (showVersion version))

----------------------
-- Main
----------------------

main :: IO ()
main = do
    opts <- cmdArgs $ modes ltcModes
    case opts of
        Fsck d -> do
            _ <- printf "Checking %s...\n" d
            hostname <- getHostName
            withStore ((openParameters d hostname) { createIfMissing = False }) (\_ -> return ())
        Info d lk -> do
            hostname <- getHostName
            withStore (openParameters d hostname) (doInfo d lk hostname)
        Export d fo -> do
            hostname <- getHostName
            withStore (openParameters d hostname) $ \store -> do
                dp <- getDiffPack store
                BL.writeFile fo (printHum (toSexp dp))
        Import d fi -> do
            hostname <- getHostName
            withStore (openParameters d hostname) $ \store -> do
                (Just dp :: Maybe DiffPack) <- fromSexp . head . parseExn <$> BL.readFile fi
                conflicts <- insertChangesInto store dp
                case conflicts of
                    [] -> return ()
                    _  -> printf "%d conflicts\n" (length conflicts)
        Populate d cnt -> do
            _ <- printf "Populating %s\n" d
            hostname <- getHostName
            withStore (openParameters d hostname) (doPopulate cnt)
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
        OpenParameters { location        = d
                       , useCompression  = False
                       , nodeName        = (BL.pack hostname)
                       , createIfMissing = True }

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
