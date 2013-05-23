{-# LANGUAGE DeriveDataTypeable #-}

module Network.NodeServer (
        module Network.Types,

        ltcPort,
        Node, shutdown,
        serve, serveWithPort,
        Connection, ConnectionId, connect, closeConnection,
        sendMessage
    ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO
                          , MVar, newMVar, withMVar, takeMVar, putMVar )
import Control.Exception ( Exception )
import Control.Monad ( unless )
import Control.Proxy
import Data.Typeable ( Typeable )
import Data.ByteString ( ByteString )
import Data.Map ( Map )
import Ltc.Store ( Store )
import Network.NodeProtocol ( NodeMessage, encode, decode )
import Network.Socket ( Socket(..), socket, sClose, bindSocket, iNADDR_ANY
                      , AddrInfo(..), getAddrInfo, defaultHints
                      , Family(..), SocketType(..), SockAddr(..)
                      , SocketOption(..), setSocketOption, defaultProtocol )
import Network.Socket.ByteString ( sendAll, sendAllTo, recvFrom )
import Network.Types
import qualified Control.Exception as CE
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Network.Socket as NS

----------------------
-- Node interface
----------------------

-- | The standard LTc port.
ltcPort :: Port
ltcPort = 3582

data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

type Handler p = (() -> Producer p (ByteString, SockAddr) IO ())
                 -> (() -> Consumer p (ByteString, SockAddr) IO ())
                 -> IO ()

newtype ConnectionId = ConnectionId Int
                       deriving ( Eq, Ord, Show )

newtype Node = Node { getNodeData :: MVar NodeData }

data NodeData = NodeData
    { getShutdown         :: IO ()
    , getNextConnectionId :: ConnectionId
    , getConnections      :: Map ConnectionId Connection
    }

-- | Start the LTc interface on the standard LTc port (3582)).
serve :: (Store s) => s -> IO Node
serve = serveWithPort ltcPort

data Connection = Connection
    { getConnectionSocket   :: Socket
    , getConnectionHostname :: Hostname
    , getConnectionPort     :: Port
    }

-- | Use a local 'Node' to connect to a remote 'Node'.  Since this is a UDP connection, we
-- don't actually /connect/ to anything; we just get a handle for the connection.
connect :: Node -> Hostname -> Port -> IO Connection
connect node hostname port = do
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
    return conn

-- | Close a `Connection` to a remote `Node`.
closeConnection :: Connection -> IO ()
closeConnection conn = do
    sClose (getConnectionSocket conn)

-- | Start the Ltc interface on the given port, backed by the given
-- store.
serveWithPort :: (Store s) => Int -> s -> IO Node
serveWithPort port store = do
    tid <- forkIO $
           CE.bracket
               (bindPort port)
               (\sock -> sClose sock)
               (\sock -> CE.handle (\(_ :: Shutdown) -> return ())
                                   (ltcHandler store (socketReader sock) (socketWriter sock)))
    let nodeData = NodeData { getShutdown         = CE.throwTo tid Shutdown
                            , getNextConnectionId = ConnectionId 1
                            , getConnections      = M.empty
                            }
    Node <$> newMVar nodeData

-- | Shutdown a running 'Node'.  Idempotent.
shutdown :: Node -> IO ()
shutdown = flip withMVar getShutdown . getNodeData

ltcHandler :: (Store s) => s -> Handler ProxyFast
ltcHandler _ p c = do
    runProxy $ p >-> ltcEchoD
                 >-> ltcEncoderD
                 >-> c

ltcEchoD :: (Proxy p, Monad m) => () -> Pipe p (ByteString, SockAddr) (NodeMessage, SockAddr) m ()
ltcEchoD () = runIdentityP $ forever $ do
    (bin, addr) <- request ()
    case decode bin of
        Nothing  -> return ()
        Just msg -> respond (msg, addr)

ltcEncoderD :: (Proxy p, Monad m) => () -> Pipe p (NodeMessage, SockAddr) (ByteString, SockAddr) m ()
ltcEncoderD () = runIdentityP $ forever $ do
    (msg, addr) <- request ()
    respond (encode msg, addr)

-- | Send a single message on a connection to a remote node.
sendMessage :: Connection -> NodeMessage -> IO ()
sendMessage conn msg = do
    sendAll (getConnectionSocket conn) (encode msg)

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

-- | Stream data from the socket.
socketReader :: (Proxy p) => Socket -> () -> Producer p (ByteString, SockAddr) IO ()
socketReader sock () = runIdentityP $ forever $ do
    (bin, addr) <- lift $ recvFrom sock 4096
    unless (BS.null bin) $ respond (bin, addr)

-- | Stream data to the socket.
socketWriter :: (Proxy p) => Socket -> () -> Consumer p (ByteString, SockAddr) IO ()
socketWriter sock () = runIdentityP $ forever $ do
    (bin, addr) <- request ()
    lift $ sendAllTo sock bin addr

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
