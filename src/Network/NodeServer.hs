{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}

module Network.NodeServer (
        module Network.Types,

        nodePort,

        -- * Nodes and serving
        Node, shutdown,
        serve, serveFromLocation,

        -- * Connections and sending
        Connection, connect, closeConnection,
        sendMessage,

        -- * Neighbours
        addNeighbour, removeNeighbour
    ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO
                          , MVar, newMVar, withMVar, readMVar, modifyMVar_ )
import Control.Concurrent.STM ( atomically, newTChanIO, readTChan )
import Control.Exception ( Exception )
import Control.Monad ( unless, forM_, forever )
import Control.Proxy ( Proxy, ProxyFast, Pipe, Producer, Consumer
                     , runProxy, lift, runIdentityP, request, respond, (>->) )
import Data.ByteString ( ByteString )
import Data.Function ( on )
import Data.Set ( Set )
import Data.Typeable ( Typeable )
import Language.Sexp ( printMach, toSexp )
import Ltc.Store ( Store(..), Event(..) )
import Network.BSD ( getHostName )
import Network.Interface ( NetworkInterface, NetworkLocation )
import Network.Interface.UDP ( UdpInterface )
import Network.NodeProtocol ( NodeMessage(..), NodeEnvelope(..), encode, decode )
import qualified Control.Exception as CE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import qualified Network.Interface as NI
import qualified Network.Interface.UDP as U
import System.Log.Logger ( debugM, warningM )
import Text.Printf ( printf )

-- Re-exported module
import Network.Types

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

-- | The abstract type of a network connection.  Note that it is parametrised by the type
-- of the network interface (e.g. 'UdpInterface').  Note also that since these connections
-- are asynchronous, there is generally no way to tell if the other side is running on
-- not.
data Connection a = (NetworkInterface a) => Connection
    { getConnectionInterface :: a
    , getConnectionLocation  :: NetworkLocation a
    }

-- | The abstract type of a node.  You make a node with the @serve*@ functions, and you
-- close a node with 'shutdown'.
newtype Node a = Node { getNodeData :: MVar (NodeData a) }

-- | The local node's view of a remote node.
data RemoteNode a = NetworkInterface a => RemoteNode
    { getRemoteLocation  :: NetworkLocation a
    , getRemoteInterface :: a
    }

instance (NetworkInterface a) => Eq (RemoteNode a) where
    (==) = (==) `on` getRemoteLocation

instance (NetworkInterface a) => Ord (RemoteNode a) where
    compare = compare `on` getRemoteLocation

-- | The node's mutable state.
data NodeData a = NodeData
    { getShutdown         :: IO ()
    , getLocation         :: NetworkLocation a
    , getNeighbours       :: Set (RemoteNode a)
    }

-- | Start the node interface on the default port.
serve :: (Store s) => s -> IO (Node UdpInterface)
serve store = do
    hostname <- getHostName
    let location = U.NetworkLocation { U.host = hostname
                                     , U.port = nodePort }
    serveFromLocation location store

-- | Start the node interface on the given port.
serveFromLocation :: (NetworkInterface a, Store s) => NetworkLocation a -> s -> IO (Node a)
serveFromLocation location store = do
    debugM tag (printf "serveFromLocation %s" (show location))

    -- Make the node data-structure
    let nodeData = NodeData { getShutdown   = error "shutdown not yet defined"
                            , getLocation   = location
                            , getNeighbours = S.empty
                            }
    node <- Node <$> newMVar nodeData

    -- Start the incoming message handler
    tidNetwork <- forkIO $
        CE.bracket
            (NI.serve location)
            (\intf -> NI.close intf)
            (\intf -> CE.handle (\(_ :: Shutdown) -> return ())
                                (nodeHandler store intf (interfaceReader intf)))

    -- Start the store event listener
    eventChannel <- newTChanIO
    addEventChannel store eventChannel
    tidPublisher <- forkIO $ forever $ do
        event <- atomically (readTChan eventChannel)
        sendEventToNeighbours node event

    -- Fill in the 'getShutdown' field of the node
    modifyMVar_ (getNodeData node) $ \ndata ->
        return (ndata { getShutdown = mapM_ (flip CE.throwTo Shutdown)
                                            [tidNetwork, tidPublisher] })

    return node

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

-- | Send a single message from the local node on a connection to a remote node.
sendMessage :: (NetworkInterface a) => Node a -> Connection a -> NodeMessage -> IO ()
sendMessage node conn msg = do
    debugM tag (printf "sending message to %s" (show (getConnectionLocation conn)))
    nodeData <- readMVar (getNodeData node)
    let envelope = NodeEnvelope { getEnvelopeSender  = getLocation nodeData
                                , getEnvelopeMessage = msg
                                }
    NI.send (getConnectionInterface conn) (encode envelope)
    debugM tag "message sent"

----------------------
-- Neighbour-set manipulation
----------------------

-- | Tell the given node that it has a neighbour at the given location.  Adding the same
-- location twice is a bad idea (which will probably lead to fd leaks).
addNeighbour :: (NetworkInterface a) => Node a -> NetworkLocation a -> IO ()
addNeighbour node location = do
    modifyMVar_ (getNodeData node) $ \nodeData -> do
        intf <- NI.connect location
        let remoteNode = RemoteNode { getRemoteLocation  = location
                                    , getRemoteInterface = intf
                                    }
        return (nodeData { getNeighbours = S.insert remoteNode (getNeighbours nodeData) })

-- | Tell the given node to not consider the node at the given location its neighbour
-- anymore.  Idempotent.
removeNeighbour :: (NetworkInterface a) => Node a -> NetworkLocation a -> IO ()
removeNeighbour node location = do
    modifyMVar_ (getNodeData node) $ \nodeData -> do
        let neighbours = getNeighbours nodeData
            (neighbours', rms) = S.partition (\r -> getRemoteLocation r /= location) neighbours
        -- There should be only one removed node, but we can't express this in the logic
        -- of Set.
        forM_ (S.toList rms) $ \removedNode ->
            NI.close (getRemoteInterface removedNode)
        return (nodeData { getNeighbours = neighbours' })

----------------------
-- Handler pipeline for incoming messages
----------------------

-- | Handle incoming node envelopes.
nodeHandler :: (NetworkInterface a, Store s)
            => s -> a -> (() -> Producer ProxyFast ByteString IO ()) -> IO ()
nodeHandler store intf p =
    runProxy $ p >-> envelopeDecoderD intf >-> handleNodeEnvelopeC store intf

-- | Stream data from the network interface..
interfaceReader :: (Proxy p, NetworkInterface a) => a -> () -> Producer p ByteString IO ()
interfaceReader intf () = runIdentityP $ forever $ do
    bin <- lift $ NI.receive intf
    unless (BS.null bin) $ respond bin

-- | Decode envelopes and pass them downstream.  Malformed inputs are discarded, and a
-- warning is emitted.
envelopeDecoderD :: (NetworkInterface a, Proxy p)
                 => a -> () -> Pipe p ByteString (NodeEnvelope a) IO ()
envelopeDecoderD _intf () = runIdentityP $ forever $ do
    bin <- request ()
    case decode bin of
        Nothing       -> lift $ warningM tag "failed to decode envelope"
        Just envelope -> respond envelope

-- | Handle incoming node envelopes.
handleNodeEnvelopeC :: (NetworkInterface a, Proxy p, Store s)
                    => s -> a -> () -> Consumer p (NodeEnvelope a) IO ()
handleNodeEnvelopeC store intf () = runIdentityP $ forever $ do
    envelope <- request ()
    lift $ debugM tag "handling envelope"
    lift $ handleNodeEnvelope store intf envelope

-- | Handle a single node envelope.
handleNodeEnvelope :: (NetworkInterface a, Store s) => s -> a -> NodeEnvelope a -> IO ()
handleNodeEnvelope _store _intf envelope@NodeEnvelope {getEnvelopeMessage = Ping _} = do
    debugM tag (printf "handling %s" (BL.unpack (printMach (toSexp (getEnvelopeMessage envelope)))))
    debugM tag "envelope handled"
handleNodeEnvelope _store _intf envelope = do
    warningM tag (printf "unknown message %s" (BL.unpack (printMach (toSexp (getEnvelopeMessage envelope)))))

----------------------
-- Change propagation
----------------------

sendEventToNeighbours :: (NetworkInterface a) => Node a -> Event -> IO ()
sendEventToNeighbours node (SetEvent key) = do
    withMVar (getNodeData node) $ \nodeData -> do
        mapM_ sendEventToNeighbour (S.toList (getNeighbours nodeData))
  where
    sendEventToNeighbour remoteNode = do
        debugM tag (printf "sending update for %s to neighbour %s"
                           (show key)
                           (show (getRemoteLocation remoteNode)))
        return ()
sendEventToNeighbours _node _event = do
    -- Uninteresting event
    return ()
