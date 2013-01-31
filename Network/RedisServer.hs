module Network.RedisServer (
        serve
    ) where

import Ltc.Store ( Store )
import Network.Redis

import Control.Concurrent ( forkIO )
import qualified Control.Exception as CE
import Control.Monad ( unless )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 ( ByteString )
import Control.Proxy
import Control.Proxy.Attoparsec ( parserInputD, parserD )
import Network.Socket ( Socket, socket, accept, sClose, bindSocket
                      , listen, maxListenQueue, iNADDR_ANY
                      , Family(..), SocketType(..), SockAddr(..)
                      , SocketOption(..), setSocketOption, defaultProtocol )
import Network.Socket.ByteString ( sendAll, recv )

type Handler p = (() -> Producer p ByteString IO ())
                 -> (() -> Consumer p ByteString IO ())
                 -> IO ()

serve :: (Store s) => s -> IO ()
serve store = do
    lsocket <- bindPort redisPort
    runSocketServer lsocket redisHandler

redisHandler :: Handler ProxyFast
redisHandler p c =
    runProxy $ p >-> parserInputD
                 >-> parserD redisParser
                 >-> redisProxy
                 >-> redisEncoderD
                 >-> c

redisProxy :: (Proxy p) => () -> Pipe p RedisMessage RedisMessage IO ()
redisProxy () = runIdentityP $ forever $ do
    cmd <- request ()
    let reply = cmd
    lift $ print reply
    respond reply

redisEncoderD :: (Proxy p, Monad m) => () -> Pipe p RedisMessage ByteString m ()
redisEncoderD () = runIdentityP $ forever $ do
    reply <- request ()
    respond (redisEncode reply)

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
