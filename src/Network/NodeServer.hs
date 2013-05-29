{-# LANGUAGE DeriveDataTypeable #-}

module Network.NodeServer (
        module Network.Types,

        nodePort,
        Node, shutdown,
        serve, serveWithHostAndPort,
        Connection, ConnectionId, connect, closeConnection,
        sendMessage
    ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO
                          , MVar, newMVar, withMVar, takeMVar, putMVar, readMVar )
import Control.Exception ( Exception )
import Control.Monad ( unless )
import Control.Proxy
import Data.ByteString ( ByteString )
import Data.Map ( Map )
import Data.Set ( Set )
import Data.Typeable ( Typeable )
import Language.Sexp ( printMach, toSexp )
import Ltc.Store ( Store )
import Network.BSD ( getHostName )
import Network.NodeProtocol ( NodeMessage(..), NodeEnvelope(..), encode, decode )
import Network.Socket ( Socket(..), socket, sClose, bindSocket, iNADDR_ANY
                      , AddrInfo(..), getAddrInfo, defaultHints
                      , Family(..), SocketType(..), SockAddr(..)
                      , SocketOption(..), setSocketOption, defaultProtocol )
import Network.Socket.ByteString ( sendAll, recvFrom )
import Network.Types ( Hostname, Port )
import qualified Control.Exception as CE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Network.Socket as NS
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

newtype ConnectionId = ConnectionId Int
                       deriving ( Eq, Ord, Show )

newtype Node = Node { getNodeData :: MVar NodeData }

-- | The local node's view of a remote node.
data RemoteNode = RemoteNode

data NodeData = NodeData
    { getShutdown         :: IO ()
    , getNextConnectionId :: ConnectionId
    , getConnections      :: Map ConnectionId Connection
    , getPort             :: Port
    , getHostname         :: Hostname
    , getNeighbours       :: Set RemoteNode
    }

-- | Start the node interface on the default port.
serve :: (Store s) => s -> IO Node
serve store = do
    hostname <- getHostName
    serveWithHostAndPort hostname nodePort store

-- | Start the node interface on the given port.
serveWithHostAndPort :: (Store s) => Hostname -> Port -> s -> IO Node
serveWithHostAndPort hostname port store = do
    debugM tag (printf "serveWithHostAndPort %s:%d" hostname port)
    tid <- forkIO $
           CE.bracket
               (bindPort port)
               (\sock -> sClose sock)
               (\sock -> CE.handle (\(_ :: Shutdown) -> return ())
                                   (nodeHandler store (socketReader sock)))
    let nodeData = NodeData { getShutdown         = CE.throwTo tid Shutdown
                            , getNextConnectionId = ConnectionId 1
                            , getConnections      = M.empty
                            , getPort             = port
                            , getHostname         = hostname
                            , getNeighbours       = S.empty
                            }
    Node <$> newMVar nodeData

-- | Shutdown a running 'Node'.  Idempotent.
shutdown :: Node -> IO ()
shutdown node = do
    withMVar (getNodeData node) $ \nodeData -> do
        debugM tag (printf "shutdown %s:%d" (getHostname nodeData) (getPort nodeData))
        getShutdown nodeData

data Connection = Connection
    { getConnectionSocket   :: Socket
    , getConnectionHostname :: Hostname
    , getConnectionPort     :: Port
    }

-- | Use a local 'Node' to connect to a remote 'Node'.  Since this is a UDP connection, we
-- don't actually /connect/ to anything; we just get a handle for the connection.
connect :: Node -> Hostname -> Port -> IO Connection
connect node hostname port = do
    debugM tag (printf "connecting to %s:%d" hostname port)
    sock <- getSocket hostname port
    nodeData <- takeMVar (getNodeData node)
    let conn = Connection { getConnectionSocket   = sock
                          , getConnectionHostname = hostname
                          , getConnectionPort     = port }
        connId = getNextConnectionId nodeData
        nodeData' = nodeData { getNextConnectionId = nextConnectionId connId
                             , getConnections      = M.insert connId conn (getConnections nodeData)
                             }
    putMVar (getNodeData node) nodeData'
    debugM tag "connected"
    return conn

-- | Close a `Connection` to a remote `Node`.
closeConnection :: Connection -> IO ()
closeConnection conn = do
    debugM tag (printf "closing connection to %s:%d"
                       (getConnectionHostname conn)
                       (getConnectionPort conn))
    sClose (getConnectionSocket conn)
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
sendMessage :: Node -> Connection -> NodeMessage -> IO ()
sendMessage node conn msg = do
    debugM tag (printf "sending message to %s:%d"
                       (getConnectionHostname conn)
                       (getConnectionPort conn))
    nodeData <- readMVar (getNodeData node)
    let envelope = NodeEnvelope { getEnvelopeSender  = (getHostname nodeData, getPort nodeData)
                                , getEnvelopeMessage = msg
                                }
    sendAll (getConnectionSocket conn) (encode envelope)
    debugM tag "message sent"

----------------------
-- Sockets
----------------------

-- | Create a UDP socket and bind it to the given port.
bindPort :: Port -> IO Socket
bindPort port = do
    CE.bracketOnError
        (socket AF_INET Datagram defaultProtocol)
        sClose
        (\sock -> do
            -- FIXME See the examples at the end of Network.Socket.ByteString
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
            return sock)

-- | Stream data from the UDP socket.
socketReader :: (Proxy p) => Socket -> () -> Producer p ByteString IO ()
socketReader sock () = runIdentityP $ forever $ do
    (bin, _) <- lift $ recvFrom sock 4096
    unless (BS.null bin) $ respond bin

-- | Create a socket connected to the given network address.
getSocket :: Hostname -> Port -> IO Socket
getSocket hostname port = do
    addrInfos <- getAddrInfo (Just (defaultHints { addrFamily = AF_INET }))
                             (Just hostname)
                             (Just $ show port)
    CE.bracketOnError
        (socket AF_INET Datagram defaultProtocol)
        sClose
        (\s -> do
             NS.connect s (addrAddress $ head addrInfos)
             return s)

----------------------
-- Helpers
----------------------

nextConnectionId :: ConnectionId -> ConnectionId
nextConnectionId (ConnectionId n) = ConnectionId (n + 1)
