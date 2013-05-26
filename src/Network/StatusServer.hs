{-# LANGUAGE DeriveDataTypeable #-}

module Network.StatusServer (
        module Network.Types,

        Status, shutdown,
        serve, statusPort
    ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( atomically, newTChanIO, readTChan )
import Control.Exception ( Exception )
import Control.Monad ( forever )
import Data.Aeson ( encode )
import Data.Monoid ( mempty )
import Data.Typeable ( Typeable )
import Ltc.Store ( Store(..), EventChannel )
import Network.Types ( Hostname, Port )
import Network.WebSockets ( WebSockets, Hybi00, textData )
import Network.WebSockets.Snap ( runWebSocketsSnap )
import Network.WebSockets.Util.PubSub ( PubSub, newPubSub, subscribe, publish )
import qualified Control.Exception as CE
import Snap.Core ( route )
import Snap.Http.Server ( ConfigLog(..), setErrorLog, setAccessLog
                        , httpServe, setPort )
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

-- | The standard status port.
statusPort :: Port
statusPort = 8000

data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

data Status = Status
    { getShutdown     :: IO ()
    , getEventChannel :: EventChannel
    , getPubSub       :: PubSub Hybi00
    }

-- | Start the status interface.
serve :: (Store s) => s -> IO Status
serve store = do
    debugM tag "starting status interface on 8000"
    pubSub <- newPubSub
    let handler = route [ ("", indexHandler)
                        , ("r", resourcesHandler)
                        , ("status", statusHandler pubSub)
                        ]
        config = setAccessLog ConfigNoLog $
                 setErrorLog ConfigNoLog $
                 setPort statusPort $
                 mempty
    tid <- forkIO $
           CE.handle (\(_ :: Shutdown) -> return ()) $ do
               httpServe config handler
    eventChannel <- newTChanIO
    addEventChannel store eventChannel
    tidPublisher <- forkIO $ forever $ do
        event <- atomically (readTChan eventChannel)
        publish pubSub (textData (encode event))
    let doShutdown = mapM_ (flip CE.throwTo Shutdown) [tidPublisher, tid]
    let status = Status { getShutdown     = doShutdown
                        , getEventChannel = eventChannel
                        , getPubSub       = pubSub
                        }
    return status
  where
    indexHandler = serveFile "www/index.html"
    resourcesHandler = serveDirectory "www/r"
    statusHandler pubSub = runWebSocketsSnap $ \_req -> do
        subscribe pubSub :: WebSockets Hybi00 ()

-- | Shutdown a running 'Status'.  Idempotent.
shutdown :: Status -> IO ()
shutdown = getShutdown
