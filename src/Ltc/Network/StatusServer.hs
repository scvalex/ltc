{-# LANGUAGE DeriveDataTypeable #-}

module Ltc.Network.StatusServer (
        module Ltc.Network.Types,

        Status, shutdown,
        serve, serveWithPort, statusPort
    ) where

import Control.Applicative ( (<|>) )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM ( atomically, newTChanIO, readTChan )
import Control.Exception ( Exception )
import Control.Monad ( forever )
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( encode )
import Data.ByteString.Lazy.Char8 ( ByteString, fromStrict )
import Data.Monoid ( mempty )
import Data.Typeable ( Typeable )
import Ltc.Monkey ( Monkey )
import Ltc.Store ( Store(..), EventChannel )
import Network.WebSockets ( WebSockets, Hybi00, textData, acceptRequest )
import Network.WebSockets.Snap ( runWebSocketsSnap )
import Network.WebSockets.Util.PubSub ( PubSub, newPubSub, subscribe, publish )
import qualified Control.Exception as CE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Ltc.Monkey as M
import Snap.Core ( Method(..), method, route
                 , writeLBS
                 , getRequest, rqPostParam )
import Snap.Http.Server ( ConfigLog(..), setErrorLog, setAccessLog
                        , httpServe, setPort )
import Snap.Util.FileServe ( serveFile, serveDirectory )
import System.Log.Logger ( debugM, warningM )
import Text.Printf ( printf )

-- Re-exported module
import Ltc.Network.Types

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
serve :: (Store s) => s -> Maybe Monkey -> IO Status
serve = serveWithPort statusPort

-- | Start the status interface on the given port.
serveWithPort :: (Store s) => Port -> s -> Maybe Monkey -> IO Status
serveWithPort port store mmonkey = do
    debugM tag (printf "starting status interface on %d" port)
    pubSub <- newPubSub
    let handler = route ([ ("", indexHandler)
                         , ("r", resourcesHandler)
                         , ("status", statusHandler pubSub)
                         ]
                         ++ maybe [] (\monkey ->
                                       [("monkey", monkeyHandler monkey)]) mmonkey)
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

    monkeyHandler monkey =
        route [ ("active", monkeySetActiveHandler monkey <|> monkeyGetActiveHandler monkey)
              , ("delay", monkeySetDelayHandler monkey <|> monkeyGetDelayHandler monkey)
              ]
    monkeyGetActiveHandler monkey = method GET $ do
        active <- liftIO $ M.getActive monkey
        writeLBS (encode active)
    monkeySetActiveHandler monkey = method POST $ do
        rq <- getRequest
        case rqPostParam "state" rq of
            Just [stateJ] -> do
                case maybeRead (fromStrict stateJ) of
                    Nothing -> do
                        liftIO $ warningM tag (printf "could not decode monkey set active param '%s'"
                                                      (show stateJ))
                    Just stateI -> do
                        let state = toEnum stateI
                        liftIO $ debugM tag (printf "changed monkey state to %s" (show state))
                        liftIO $ M.setActive monkey state
            stateParam ->
                liftIO $ warningM tag (printf "could not interpret monkey set active param '%s'"
                                              (show stateParam))

    monkeyGetDelayHandler monkey = method GET $ do
        delay <- liftIO $ M.getDelay monkey
        writeLBS (encode delay)
    monkeySetDelayHandler monkey = method POST $ do
        rq <- getRequest
        case rqPostParam "delay" rq of
            Just [delayJ] -> do
                case maybeRead (fromStrict delayJ) of
                    Nothing -> do
                        liftIO $ warningM tag (printf "could not decode monkey set delay param '%s'"
                                                      (show delayJ))
                    Just delay -> do
                        liftIO $ debugM tag (printf "changed monkey delay to %s" (show delay))
                        liftIO $ M.setDelay monkey (delay !! 0, delay !! 1)
            delayParam ->
                liftIO $ warningM tag (printf "could not interpret monkey set delay param '%s'"
                                              (show delayParam))


-- | Shutdown a running 'Status'.  Idempotent.
shutdown :: Status -> IO ()
shutdown = getShutdown

--------------------------------
-- Helpers
--------------------------------

-- | Try to read a value; return 'Nothing' if the parse fails.
maybeRead :: (Read a) => ByteString -> Maybe a
maybeRead bs =
    let s = BL.unpack bs in
    case readsPrec 0 s of
        [(x, "")] -> Just x
        _         -> Nothing
