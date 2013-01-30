module Network.RedisServer (
        serve
    ) where

import Network.Redis

import Control.Concurrent ( forkIO )
import qualified Control.Exception as CE
import Control.Monad ( forever, unless )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.Char8 ( ByteString )
import Control.Proxy
import Network.Socket ( Socket, socket, accept, sClose, bindSocket
                      , listen, maxListenQueue, iNADDR_ANY
                      , Family(..), SocketType(..), SockAddr(..)
                      , SocketOption(..), setSocketOption, defaultProtocol )
import Network.Socket.ByteString ( sendAll, recv )

type Handler p = (() -> Producer p ByteString IO ())
                 -> (() -> Consumer p ByteString IO ())
                 -> IO ()

serve :: IO ()
serve = do
    lsocket <- bindPort redisPort
    runSocketServer lsocket redisHandler

redisHandler :: Handler ProxyFast
redisHandler p c = runProxy $ p >-> redisProxy >-> c

redisProxy :: (Proxy p) => () -> Pipe p ByteString ByteString IO ()
redisProxy () = runIdentityP $ forever $ do
    cmd <- request ()
    let reply = cmd
    respond reply

----------------------
-- Sockets
----------------------

type Port = Int

redisPort :: Port
redisPort = 6379

-- | Create a socket and bind it to the given port.
bindPort :: Port -> IO Socket
bindPort port = do
    CE.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        sClose
        (\s -> do
            -- FIXME See the examples at the end of Network.Socket.ByteString
            setSocketOption s ReuseAddr 1
            bindSocket s (SockAddrInet (fromIntegral port) iNADDR_ANY)
            listen s maxListenQueue
            return s)

runSocketServer :: (Proxy p) => Socket -> Handler p -> IO ()
runSocketServer lsocket handler = forever $ do
    (socket, _addr) <- accept lsocket
    _ <- forkIO $ CE.finally
                      (handler (socketReader socket) (socketWriter socket))
                      (sClose socket)
    return ()

-- | Stream data from the socket.
socketReader :: (Proxy p) => Socket -> () -> Producer p ByteString IO ()
socketReader socket () = runIdentityP loop
  where
    loop = do
        bin <- lift $ recv socket 4096
        let bin' = BL.fromChunks [bin]
        unless (BL.null bin') $ do
            respond bin'
            loop

-- | Stream data to the socket.
socketWriter :: (Proxy p) => Socket -> () -> Consumer p ByteString IO ()
socketWriter socket () = runIdentityP $ forever $ do
    bin <- request ()
    lift $ mapM_ (sendAll socket) (BL.toChunks bin)
