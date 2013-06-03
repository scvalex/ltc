{-# LANGUAGE DeriveDataTypeable #-}

-- | A monkey is a process which randomly makes changes to the store.
module Ltc.Monkey (
        Monkey, start, shutdown
    ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Exception ( Exception )
import Control.Monad ( forever )
import Data.String ( fromString )
import Data.Typeable ( Typeable )
import Ltc.Store ( Store(..), Version )
import qualified Control.Exception as CE
import System.Log.Logger ( debugM )
import System.Random ( randomRIO )
import Text.Printf ( printf )

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "Monkey"

----------------------
-- Monkey interface
----------------------

data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

data Monkey = Monkey
    { getShutdown :: IO ()
    }

-- | Start the monkey on the given store.
start :: (Store s) => s -> IO Monkey
start store = do
    debugM tag "starting monkey"
    tid <- forkIO $
           CE.handle (\(_ :: Shutdown) -> return ()) $ do
               forever $ do
                   d <- randomRIO (200 * 1000, 400 * 1000) -- 0.2s - 0.4s
                   threadDelay d
                   accessStoreRandomly
    let monkey = Monkey { getShutdown = CE.throwTo tid Shutdown
                        }
    return monkey
  where
    -- FIXME Monkey should use String values as well.
    accessStoreRandomly = do
        n <- randomRIO (1, 4 :: Int)
        if n <= 3               -- 75% chance of read
            then readStoreRandomly
            else writeStoreRandomly

    readStoreRandomly = do
        key <- randomKey
        (_ :: Maybe (Integer, Version)) <- getLatest store key
        return ()

    writeStoreRandomly = do
        key <- randomKey
        v <- randomRIO (1, 4 :: Integer)
        _ <- set store key v
        return ()

    randomKey = do
        i <- randomRIO (1, 8 :: Int)
        return (fromString (printf "i%03d" i))

-- | Shutdown the given monkey.
shutdown :: Monkey -> IO ()
shutdown monkey = do
    debugM tag "shutting monkey down"
    getShutdown monkey
