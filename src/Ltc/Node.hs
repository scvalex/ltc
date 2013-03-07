{-# LANGUAGE DeriveDataTypeable #-}

module Ltc.Node (
        serve, serveWithPort
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
                      , Family(..), SocketType(..), SockAddr(..), accept
                      , SocketOption(..), setSocketOption, defaultProtocol )
import Network.Socket.ByteString ( sendAll, recv )
import qualified Control.Exception as CE
import qualified Data.ByteString as BS

ltcPort :: Port
ltcPort = 6379

data Shutdown = Shutdown
              deriving ( Show, Typeable )

instance Exception Shutdown

type Handler p = (() -> Producer p ByteString IO ())
                 -> (() -> Consumer p ByteString IO ())
                 -> IO ()

serve :: (Store s) => s -> IO (IO ())
serve = serveWithPort ltcPort

serveWithPort :: (Store s) => Int -> s -> IO (IO ())
serveWithPort port store = do
    tid <- forkIO $
           CE.bracket
               (bindPort port)
               (\lsocket -> sClose lsocket)
               (\lsocket -> CE.handle (\(_ :: Shutdown) -> return ())
                                      (runSocketServer lsocket (ltcHandler store)))
    return (CE.throwTo tid Shutdown)

ltcHandler :: (Store s) => s -> Handler ProxyFast
ltcHandler = undefined

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
        (\s -> do
            -- FIXME See the examples at the end of Network.Socket.ByteString
            setSocketOption s ReuseAddr 1
            bindSocket s (SockAddrInet (fromIntegral port) iNADDR_ANY)
            return s)

runSocketServer :: (Proxy p) => Socket -> Handler p -> IO ()
runSocketServer lsocket handler = forever $ do
    (sock, _addr) <- accept lsocket
    _ <- forkIO $ CE.finally
                      (handler (socketReader sock) (socketWriter sock))
                      (sClose sock)
    return ()

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
