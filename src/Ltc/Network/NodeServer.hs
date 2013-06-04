{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}

module Ltc.Network.NodeServer (
        module Ltc.Network.Types,

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
import Control.Monad ( unless, forever )
import Control.Proxy ( Proxy, ProxyFast, Pipe, Producer, Consumer
                     , runProxy, lift, runIdentityP, request, respond, (>->) )
import Data.ByteString ( ByteString )
import Data.Function ( on )
import Data.Map ( Map )
import Data.Typeable ( Typeable )
import Language.Sexp ( printMach, toSexp )
import Ltc.Network.Interface ( NetworkInterface, NetworkLocation )
import Ltc.Network.Interface.UDP ( UdpInterface )
import Ltc.Network.NodeProtocol ( NodeMessage(..), NodeEnvelope(..), encode, decode )
import Ltc.Store ( Store(..), Event(..), SetEvent(..), Version, NodeName )
import Network.BSD ( getHostName )
import qualified Control.Exception as CE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Data.VectorClock as VC
import qualified Ltc.Network.Interface as NI
import qualified Ltc.Network.Interface.UDP as U
import System.Log.Logger ( debugM, warningM )
import Text.Printf ( printf )

-- Re-exported module
import Ltc.Network.Types

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
    , getRemoteName      :: NodeName
    , getRemoteInterface :: a
    , getRemoteClock     :: Version
    }

instance (NetworkInterface a) => Eq (RemoteNode a) where
    (==) = (==) `on` getRemoteLocation

instance (NetworkInterface a) => Ord (RemoteNode a) where
    compare = compare `on` getRemoteLocation

-- | The node's mutable state.
data NodeData a = NodeData
    { getShutdown   :: IO ()
    , getLocation   :: NetworkLocation a
    , getNodeName   :: NodeName
    , getNeighbours :: Map NodeName (RemoteNode a)
    }

-- | Start the node interface on the default port.
serve :: (Store s) => s -> NodeName -> IO (Node UdpInterface)
serve store nodeName = do
    hostname <- getHostName
    let location = U.UdpLocation { U.host = hostname
                                 , U.port = nodePort }
    serveFromLocation location store nodeName

-- | Start the node interface on the given port.
serveFromLocation :: (NetworkInterface a, Store s)
                  => NetworkLocation a -> s -> NodeName -> IO (Node a)
serveFromLocation location store nodeName = do
    debugM tag (printf "serveFromLocation %s" (show location))

    -- Make the node data-structure
    let nodeData = NodeData { getShutdown   = error "shutdown not yet defined"
                            , getLocation   = location
                            , getNodeName   = nodeName
                            , getNeighbours = M.empty
                            }
    node <- Node <$> newMVar nodeData

    -- Start the incoming message handler
    tidNetwork <- forkIO $
        CE.bracket
            (NI.serve location)
            (\intf -> NI.close intf)
            (\intf -> CE.handle (\(_ :: Shutdown) -> return ())
                                (nodeHandler node store intf (interfaceReader intf)))

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
    let envelope = NodeEnvelope { getEnvelopeLocation = getLocation nodeData
                                , getEnvelopeNode     = getNodeName nodeData
                                , getEnvelopeMessage  = msg
                                }
    NI.send (getConnectionInterface conn) (encode envelope)
    debugM tag "message sent"

----------------------
-- Neighbour-set manipulation
----------------------

-- | Tell the given node that it has a neighbour at the given location.  Adding the same
-- location twice is a bad idea (which will probably lead to fd leaks).
addNeighbour :: (NetworkInterface a) => Node a -> NodeName -> NetworkLocation a -> IO ()
addNeighbour node nodeName location = do
    modifyMVar_ (getNodeData node) $ \nodeData -> do
        intf <- NI.connect location
        let remoteNode = RemoteNode { getRemoteLocation  = location
                                    , getRemoteName      = nodeName
                                    , getRemoteInterface = intf
                                    , getRemoteClock     = VC.empty
                                    }
            neighbours' = M.insert nodeName remoteNode (getNeighbours nodeData)
        return (nodeData { getNeighbours = neighbours' })

-- | Tell the given node to not consider the node at the given location its neighbour
-- anymore.  Idempotent.
removeNeighbour :: (NetworkInterface a) => Node a -> NodeName -> IO ()
removeNeighbour node nodeName = do
    modifyMVar_ (getNodeData node) $ \nodeData -> do
        let neighbours = getNeighbours nodeData
            mremoved = M.lookup nodeName neighbours
            neighbours' = M.delete nodeName neighbours
        case mremoved of
             Nothing      -> return ()
             Just removed -> NI.close (getRemoteInterface removed)
        return (nodeData { getNeighbours = neighbours' })

----------------------
-- Handler pipeline for incoming messages
----------------------

-- | Handle incoming node envelopes.
nodeHandler :: (NetworkInterface a, Store s)
            => Node a -> s -> a -> (() -> Producer ProxyFast ByteString IO ()) -> IO ()
nodeHandler node store intf p =
    runProxy $ p >-> envelopeDecoderD intf >-> handleNodeEnvelopeC node store

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
handleNodeEnvelopeC :: (Proxy p, Store s)
                    => Node a -> s -> () -> Consumer p (NodeEnvelope a) IO ()
handleNodeEnvelopeC node store () = runIdentityP $ forever $ do
    envelope <- request ()
    lift $ debugM tag (printf "handling %s"
                              (BL.unpack (printMach (toSexp (getEnvelopeMessage envelope)))))
    lift $ handleNodeEnvelope node store envelope

-- | Handle a single node envelope.
handleNodeEnvelope :: (Store s) => Node a -> s -> NodeEnvelope a -> IO ()
handleNodeEnvelope _node _store (NodeEnvelope {getEnvelopeMessage = Ping _}) = do
    debugM tag "ping handled"
handleNodeEnvelope node _store envelope@(NodeEnvelope {getEnvelopeMessage = changes@(Changes {})}) = do
    modifyMVar_ (getNodeData node) $ \nodeData -> do
        let neighbours' = M.adjust (\remoteNode -> remoteNode { getRemoteClock =
                                                                     getVersionClock changes})
                                   (getEnvelopeNode envelope)
                                   (getNeighbours nodeData)
        return (nodeData { getNeighbours = neighbours' })
    debugM tag "changes handled"

----------------------
-- Change propagation
----------------------

sendEventToNeighbours :: (NetworkInterface a) => Node a -> Event -> IO ()
sendEventToNeighbours node (MSetEvent setEvents) = do
    withMVar (getNodeData node) $ \nodeData -> do
        mapM_ sendEventToNeighbour (M.elems (getNeighbours nodeData))
  where
    sendEventToNeighbour remoteNode = do
        debugM tag (printf "sending update for %s to neighbour %s"
                           (show (map setKey setEvents))
                           (show (getRemoteLocation remoteNode)))
        return ()
sendEventToNeighbours _node _event = do
    -- Uninteresting event
    return ()
