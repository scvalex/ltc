{-# LANGUAGE DeriveDataTypeable #-}

module Network.StatusServer (
        module Network.Types,

        Status, shutdown,
        serve
    ) where

import Control.Concurrent ( forkIO )
import Control.Exception ( Exception )
import Data.Typeable ( Typeable )
import Ltc.Store ( Store )
import Network.Types ( Hostname, Port )
import Network.WebSockets ( WebSockets, Hybi00 )
import Network.WebSockets.Snap ( runWebSocketsSnap )
import qualified Control.Exception as CE
import Snap.Core ( route )
import Snap.Http.Server ( httpServe, defaultConfig )
import Snap.Util.FileServe ( serveFile, serveDirectory )
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

data Status = Status
    { getShutdown :: IO ()
    }

-- | Start the status interface.
serve :: (Store s) => s -> IO Status
serve _store = do
    debugM tag "starting status interface"
    let handler = route [ ("", indexHandler)
                        , ("r", resourcesHandler)
                        , ("status", statusHandler)
                        ]
    tid <- forkIO $
           CE.handle (\(_ :: Shutdown) -> return ()) $ do
               httpServe defaultConfig handler
    let status = Status { getShutdown = CE.throwTo tid Shutdown
                        }
    return status
  where
    indexHandler = serveFile "www/index.html"
    resourcesHandler = serveDirectory "www/r"
    statusHandler = runWebSocketsSnap $ \_req -> do
        undefined :: WebSockets Hybi00 ()

-- | Shutdown a running 'Status'.  Idempotent.
shutdown :: Status -> IO ()
shutdown = getShutdown
