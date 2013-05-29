{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}

module Network.NodeServer (
        module Network.Types,

        nodePort,
        Node, shutdown,
        serve, serveFromLocation,
        Connection, connect, closeConnection,
        sendMessage
    ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO
                          , MVar, newMVar, withMVar )
import Control.Exception ( Exception )
import Control.Monad ( unless )
import Control.Proxy
import Data.ByteString ( ByteString )
import Data.Set ( Set )
import Data.Typeable ( Typeable )
import Language.Sexp ( printMach, toSexp )
import Ltc.Store ( Store )
import Network.BSD ( getHostName )
import Network.Interface ( NetworkInterface, NetworkLocation )
import Network.Interface.UDP ( UDPInterface )
import Network.NodeProtocol ( NodeMessage(..), NodeEnvelope(..), encode, decode )
import Network.Types ( Hostname, Port )
import qualified Control.Exception as CE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import qualified Network.Interface as NI
import qualified Network.Interface.UDP as UDP
import System.Log.Logger ( debugM, warningM )
import Text.Printf ( printf )

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "NodeServer"

----------------------
-- Node interface
----------------------

-- | The default node port.
nodePort :: Port
nodePort = 3582

data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

data Connection a = (NetworkInterface a) => Connection
    { getConnectionInterface :: a
    , getConnectionLocation  :: NetworkLocation a
    }

newtype Node a = Node { getNodeData :: MVar (NodeData a) }

-- | The local node's view of a remote node.
data RemoteNode a = RemoteNode

data NodeData a = NodeData
    { getShutdown         :: IO ()
    , getLocation         :: NetworkLocation a
    , getNeighbours       :: Set (RemoteNode a)
    }

-- | Start the node interface on the default port.
serve :: (Store s) => s -> IO (Node UDPInterface)
serve store = do
    hostname <- getHostName
    let location = UDP.NetworkLocation { UDP.host = hostname
                                       , UDP.port = nodePort }
    serveFromLocation location store

-- | Start the node interface on the given port.
serveFromLocation :: (Store s, NetworkInterface a) => NetworkLocation a -> s -> IO (Node a)
serveFromLocation location store = do
    debugM tag (printf "serveFromLocation %s" (show location))
    tid <- forkIO $
           CE.bracket
               (NI.serve location)
               (\intf -> NI.close intf)
               (\intf -> CE.handle (\(_ :: Shutdown) -> return ())
                                   (nodeHandler store (interfaceReader intf)))
    let nodeData = NodeData { getShutdown         = CE.throwTo tid Shutdown
                            , getLocation         = location
                            , getNeighbours       = S.empty
                            }
    Node <$> newMVar nodeData

-- | Shutdown a running 'Node'.  Idempotent.
shutdown :: (NetworkInterface a) => Node a -> IO ()
shutdown node = do
    withMVar (getNodeData node) $ \nodeData -> do
        debugM tag (printf "shutdown %s" (show (getLocation nodeData)))
        getShutdown nodeData

-- | Use a local 'Node' to connect to a remote 'Node'.  Since this is an asynchronous
-- connection, we don't actually /connect/ to anything; we just get a handle for the
-- connection.
connect :: (NetworkInterface a) => Node a -> NetworkLocation a -> IO (Connection a)
connect _node location = do
    debugM tag (printf "connecting to %s" (show location))
    intf <- NI.connect location
    let conn = Connection { getConnectionInterface = intf
                          , getConnectionLocation  = location }
    debugM tag "connected"
    return conn

-- | Close a `Connection` to a remote `Node`.
closeConnection :: (NetworkInterface a) => Connection a -> IO ()
closeConnection conn = do
    debugM tag (printf "closing connection to %s" (show (getConnectionLocation conn)))
    NI.close (getConnectionInterface conn)
    debugM tag "closed connection"

-- | Handle incoming node envelopes.
nodeHandler :: (Store s) => s -> (() -> Producer ProxyFast ByteString IO ()) -> IO ()
nodeHandler store p = runProxy $ p >-> envelopeDecoderD >-> handleNodeEnvelopeC store

-- | Decode envelopes and pass them downstream.  Malformed inputs are discarded, and a
-- warning is emitted.
envelopeDecoderD :: (Proxy p) => () -> Pipe p ByteString NodeEnvelope IO ()
envelopeDecoderD () = runIdentityP $ forever $ do
    bin <- request ()
    case decode bin of
        Nothing       -> lift $ warningM tag "failed to decode envelope"
        Just envelope -> respond envelope

-- | Handle incoming node envelopes.
handleNodeEnvelopeC :: (Proxy p, Store s) => s -> () -> Consumer p NodeEnvelope IO ()
handleNodeEnvelopeC store () = runIdentityP $ forever $ do
    envelope <- request ()
    lift $ debugM tag "handling envelope"
    lift $ handleNodeEnvelope store envelope

-- | Handle a single node envelope.
handleNodeEnvelope :: (Store s) => s -> NodeEnvelope -> IO ()
handleNodeEnvelope _store envelope@NodeEnvelope {getEnvelopeMessage = Ping _} = do
    debugM tag (printf "handling %s" (BL.unpack (printMach (toSexp envelope))))
    debugM tag "envelope handled"
handleNodeEnvelope _store envelope = do
    warningM tag (printf "unknown message %s" (BL.unpack (printMach (toSexp envelope))))

-- | Send a single message from the local node on a connection to a remote node.
sendMessage :: (NetworkInterface a) => Node a -> Connection a -> NodeMessage -> IO ()
sendMessage _node conn msg = do
    debugM tag (printf "sending message to %s" (show (getConnectionLocation conn)))
    let envelope = NodeEnvelope { getEnvelopeSender  = undefined
                                , getEnvelopeMessage = msg
                                }
    NI.send (getConnectionInterface conn) (encode envelope)
    debugM tag "message sent"

----------------------
-- Sockets
----------------------

-- | Stream data from the network interface..
interfaceReader :: (Proxy p, NetworkInterface a) => a -> () -> Producer p ByteString IO ()
interfaceReader intf () = runIdentityP $ forever $ do
    bin <- lift $ NI.receive intf
    unless (BS.null bin) $ respond bin
