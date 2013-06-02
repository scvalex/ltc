{-# LANGUAGE DeriveDataTypeable #-}

module Network.RedisServer (
        module Network.Types,

        serve, serveWithPort
    ) where

import Control.Concurrent ( forkIO )
import Control.Exception ( Exception )
import Control.Monad ( unless, forever )
import Control.Proxy ( Proxy, ProxyFast, Producer, Consumer, Pipe
                     , runProxy, lift, runIdentityP, request, respond, (>->) )
import Control.Proxy.Attoparsec ( parserInputD, parserD )
import Data.ByteString.Char8 ( ByteString )
import Data.Data ( Data, Typeable )
import Ltc.Store ( Store )
import Network.RedisAdapter ( redisProxyD )
import Network.RedisProtocol ( RedisMessage(..), redisParser, redisEncode )
import Network.Socket ( Socket, socket, accept, sClose, bindSocket
                      , Family(..), SocketType(..), SockAddr(..)
                      , listen, maxListenQueue, iNADDR_ANY
                      , SocketOption(..), setSocketOption, defaultProtocol )
import Network.Socket.ByteString ( sendAll, recv )
import qualified Control.Exception as CE
import qualified Data.ByteString.Char8 as BS
import System.Log.Logger ( debugM )
import Text.Printf ( printf )

-- Re-exported module
import Network.Types

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "RedisServer"

----------------------
-- Redis interface
----------------------

type Handler p = (() -> Producer p ByteString IO ())
                 -> (() -> Consumer p ByteString IO ())
                 -> IO ()

data Shutdown = Shutdown
              deriving ( Data, Show, Typeable )

instance Exception Shutdown

-- | Start the Redis interface on the standard Redis port (6379).
serve :: (Store s) => s -> IO (IO ())
serve = serveWithPort redisPort

-- | Start the Redis interface on the given port.
serveWithPort :: (Store s) => Int -> s -> IO (IO ())
serveWithPort port store = do
    debugM tag (printf "serveWithPort %d" port)
    tid <- forkIO $
           CE.bracket
               (bindPort port)
               (\lsocket -> sClose lsocket)
               (\lsocket -> CE.handle (\(_ :: Shutdown) -> return ())
                                      (runSocketServer lsocket (redisHandler store)))
    return $ do
        debugM tag (printf "shutdown %d" port)
        CE.throwTo tid Shutdown

redisHandler :: (Store s) => s -> Handler ProxyFast
redisHandler store p c =
    runProxy $ p >-> parserInputD
                 >-> parserD redisParser
                 >-> (redisProxyD store)
                 >-> redisEncoderD
                 >-> c

redisEncoderD :: (Proxy p, Monad m) => () -> Pipe p RedisMessage ByteString m ()
redisEncoderD () = runIdentityP $ forever $ do
    reply <- request ()
    respond (redisEncode reply)

----------------------
-- Sockets
----------------------

redisPort :: Port
redisPort = 6379

-- | Create a TCP socket and bind it to the given port.
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
