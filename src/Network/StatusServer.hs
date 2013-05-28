{-# LANGUAGE DeriveDataTypeable #-}

module Network.StatusServer (
        module Network.Types,

        Status, shutdown,
        serve, serveWithPort, statusPort
    ) where

import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( atomically, newTChanIO, readTChan )
import Control.Exception ( Exception )
import Control.Monad ( forever )
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( encode )
import Data.Monoid ( mempty )
import Data.Typeable ( Typeable )
import Ltc.Store ( Store(..), EventChannel )
import Network.Types ( Hostname, Port )
import Network.WebSockets ( WebSockets, Hybi00, textData, acceptRequest )
import Network.WebSockets.Snap ( runWebSocketsSnap )
import Network.WebSockets.Util.PubSub ( PubSub, newPubSub, subscribe, publish )
import qualified Data.ByteString.Char8 as BS
import qualified Control.Exception as CE
import Snap.Core ( route )
import Snap.Http.Server ( ConfigLog(..), setErrorLog, setAccessLog
                        , httpServe, setPort )
import Snap.Util.FileServe ( serveFile, serveDirectory )
import System.Log.Logger ( debugM )
import Text.Printf ( printf )

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "StatusServer"

----------------------
-- Status interface
----------------------

-- | The default status port.
statusPort :: Port
statusPort = 5000

data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

data Status = Status
    { getShutdown     :: IO ()
    , getEventChannel :: EventChannel
    , getPubSub       :: PubSub Hybi00
    }

-- | Start the status interface on the default port.
serve :: (Store s) => s -> IO Status
serve = serveWithPort statusPort

-- | Start the status interface on the given port.
serveWithPort :: (Store s) => Port -> s -> IO Status
serveWithPort port store = do
    debugM tag (printf "starting status interface on %d" statusPort)
    pubSub <- newPubSub
    let handler = route [ ("", indexHandler)
                        , ("r", resourcesHandler)
                        , ("status", statusHandler pubSub)
                        ]
        config = setAccessLog (ConfigIoLog BS.putStrLn) $
                 setErrorLog (ConfigIoLog BS.putStrLn) $
                 setPort port $
                 mempty
    tidHttp <- forkIO $
               CE.handle (\(_ :: Shutdown) -> return ()) $ do
                   httpServe config handler
    eventChannel <- newTChanIO
    addEventChannel store eventChannel
    tidPublisher <- forkIO $ forever $ do
        event <- atomically (readTChan eventChannel)
        publish pubSub (textData (encode event))
    let doShutdown = mapM_ (flip CE.throwTo Shutdown) [tidPublisher, tidHttp]
    let status = Status { getShutdown     = doShutdown
                        , getEventChannel = eventChannel
                        , getPubSub       = pubSub
                        }
    return status
  where
    indexHandler = serveFile "www/index.html"
    resourcesHandler = serveDirectory "www/r"
    statusHandler pubSub = runWebSocketsSnap $ \req -> do
        acceptRequest req
        liftIO $ debugM tag "new client subscription"
        subscribe pubSub :: WebSockets Hybi00 ()

-- | Shutdown a running 'Status'.  Idempotent.
shutdown :: Status -> IO ()
shutdown = getShutdown
