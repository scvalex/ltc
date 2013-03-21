{-# LANGUAGE DeriveDataTypeable #-}

module Ltc.Node (
        ltcPort, serve, serveWithPort
    ) where

import Control.Concurrent ( forkIO )
import Control.Exception ( Exception )
import Control.Monad ( unless )
import Control.Proxy
import Data.Typeable ( Typeable )
import Data.ByteString ( ByteString )
import Ltc.Store ( Store )
import Network.Socket ( Socket(..), socket, sClose, bindSocket, iNADDR_ANY
                      , AddrInfo(..), getAddrInfo, defaultHints, connect
                      , Family(..), SocketType(..), SockAddr(..)
                      , SocketOption(..), setSocketOption, defaultProtocol )
import Network.Socket.ByteString ( sendAll, recv )
import qualified Control.Exception as CE
import qualified Data.ByteString as BS

----------------------
-- Node interface
----------------------

-- | The standard LTc port.
ltcPort :: Port
ltcPort = 3582

data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

type Handler p = (() -> Producer p ByteString IO ())
                 -> (() -> Consumer p ByteString IO ())
                 -> IO ()

-- | Start the LTc interface on the standard LTc port (3582)).
serve :: (Store s) => s -> IO (IO ())
serve = serveWithPort ltcPort

-- | Start the Ltc interface on the given port, backed by the given
-- store.
serveWithPort :: (Store s) => Int -> s -> IO (IO ())
serveWithPort port store = do
    tid <- forkIO $
           CE.bracket
               (bindPort port)
               (\sock -> sClose sock)
               (\sock -> CE.handle (\(_ :: Shutdown) -> return ())
                                   (ltcHandler store (socketReader sock) (socketWriter sock)))
    return (CE.throwTo tid Shutdown)

ltcHandler :: (Store s) => s -> Handler ProxyFast
ltcHandler store p c =
    runProxy $ p >-> c

----------------------
-- Sockets
----------------------

type Port = Int

type Hostname = String

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
socketReader :: (Proxy p) => Socket -> () -> Producer p ByteString IO ()
socketReader sock () = runIdentityP loop
  where
    loop = do
        bin <- lift $ recv sock 4096
        unless (BS.null bin) $ do
            respond bin
            loop

-- | Stream data to the socket.
socketWriter :: (Proxy p) => Socket -> () -> Consumer p ByteString IO ()
socketWriter sock () = runIdentityP $ forever $ do
    bin <- request ()
    lift $ sendAll sock bin

-- | Create a socket connected to the given network address.
getSocket :: Hostname -> Int -> IO Socket
getSocket hostname port = do
    addrInfos <- getAddrInfo (Just (defaultHints { addrFamily = AF_INET }))
                             (Just hostname)
                             (Just $ show port)
    CE.bracketOnError
        (socket AF_INET Datagram defaultProtocol)
        sClose
        (\s -> do
             connect s (addrAddress $ head addrInfos)
             return s)
