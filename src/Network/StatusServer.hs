{-# LANGUAGE DeriveDataTypeable #-}

module Network.StatusServer (
        module Network.Types,

        Status, shutdown,
        serve
    ) where

import Control.Exception ( Exception )
import Data.Typeable ( Typeable )
import Ltc.Store ( Store )
import Network.Types ( Hostname, Port )
import System.Log.Logger ( debugM )

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "StatusServer"

----------------------
-- Status interface
----------------------

data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

newtype Status = Status { getShutdown :: IO () }

-- | Start the status interface.
serve :: (Store s) => s -> IO Status
serve _store = do
    debugM tag "starting status interface"
    return (Status { getShutdown = undefined })

-- | Shutdown a running 'Status'.  Idempotent.
shutdown :: Status -> IO ()
shutdown = getShutdown
