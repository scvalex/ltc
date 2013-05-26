{-# LANGUAGE DeriveDataTypeable #-}

-- | A monkey is a process which randomly makes changes to the store.
module Ltc.Monkey (
        Monkey, start, shutdown
    ) where

import Control.Concurrent ( forkIO )
import Control.Exception ( Exception )
import Data.Typeable ( Typeable )
import Ltc.Store ( Store(..) )
import qualified Control.Exception as CE
import System.Log.Logger ( debugM )

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
start _store = do
    debugM tag "starting monkey"
    tid <- forkIO $
           CE.handle (\(_ :: Shutdown) -> return ()) $ do
               return ()
    let monkey = Monkey { getShutdown = CE.throwTo tid Shutdown
                        }
    return monkey

-- | Shutdown the given monkey.
shutdown :: Monkey -> IO ()
shutdown monkey = do
    debugM tag "shutting monkey down"
    getShutdown monkey
