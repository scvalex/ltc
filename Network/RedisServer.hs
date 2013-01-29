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
import Network.Socket ( Socket, accept, sClose )
import Network.Socket.ByteString ( sendAll, recv )

serve :: IO ()
serve = undefined

----------------------
-- Sockets
----------------------

type Handler p = Producer p ByteString IO () -> Consumer p ByteString IO () -> IO ()

runSocketServer :: (Proxy p) => Socket -> Handler p -> IO ()
runSocketServer lsocket handler = forever $ do
    (socket, _addr) <- accept lsocket
    _ <- forkIO $ CE.finally
                      (handler (socketReader socket) (socketWriter socket))
                      (sClose socket)
    return ()

-- | Stream data from the socket.
socketReader :: (Proxy p) => Socket -> Producer p ByteString IO ()
socketReader socket = runIdentityP loop
  where
    loop = do
        bin <- lift $ recv socket 4096
        let bin' = BL.fromChunks [bin]
        unless (BL.null bin') $ do
            respond bin'
            loop

-- | Stream data to the socket.
socketWriter :: (Proxy p) => Socket -> Consumer p ByteString IO ()
socketWriter socket = runIdentityP $ forever $ do
    bin <- request ()
    lift $ mapM_ (sendAll socket) (BL.toChunks bin)
