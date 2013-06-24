{-# LANGUAGE DeriveDataTypeable #-}

-- | A monkey is a process which randomly makes changes to the store.
module Ltc.Monkey (
        -- * Basic interface
        Monkey, start, shutdown,

        -- * Configuration
        getDelay, setDelay, getActive, setActive
    ) where

import Control.Concurrent ( forkIO, threadDelay
                          , MVar, newMVar, readMVar, modifyMVar_ )
import Control.Exception ( Exception )
import Control.Monad ( forever, when )
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
    { getShutdown  :: IO ()
    , getDelayVar  :: MVar (Double, Double)
    , getActiveVar :: MVar Bool
    }

-- | Start the monkey on the given store.
start :: (Store s) => s -> IO Monkey
start store = do
    debugM tag "starting monkey"
    delayVar <- newMVar (1.0, 2.0)
    activeVar <- newMVar True
    tid <- forkIO $
           CE.handle (\(_ :: Shutdown) -> return ()) $ do
               forever $ do
                   (minDelay, maxDelay) <- readMVar delayVar
                   d <- randomRIO (floor (minDelay * 1000000), floor (maxDelay * 1000000))
                   threadDelay d
                   isActive <- readMVar activeVar
                   when isActive accessStoreRandomly
    let monkey = Monkey { getShutdown  = CE.throwTo tid Shutdown
                        , getDelayVar  = delayVar
                        , getActiveVar = activeVar
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

-- | Get the current delay range.
getDelay :: Monkey -> IO (Double, Double)
getDelay = readMVar . getDelayVar

-- | Set the delay range.  This is the amount of seconds the monkey waits between
-- accesses.
setDelay :: Monkey -> (Double, Double) -> IO ()
setDelay monkey delay = modifyMVar_ (getDelayVar monkey) (const (return delay))

-- | Get the current activation state.
getActive :: Monkey -> IO Bool
getActive = readMVar . getActiveVar

-- | Set the activation state.  When inactive, the monkey does not touch the store.
setActive :: Monkey -> Bool -> IO ()
setActive monkey active = modifyMVar_ (getActiveVar monkey) (const (return active))
