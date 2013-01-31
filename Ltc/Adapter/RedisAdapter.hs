module Ltc.Adapter.RedisAdapter (
        redisProxyD
    ) where

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad ( unless )
import Control.Proxy
import Ltc.Store
import Network.Redis ( RedisMessage(..) )

redisProxyD :: (Proxy p, Store s) => s -> () -> Pipe p RedisMessage RedisMessage IO ()
redisProxyD store () = runIdentityP loop
  where
    loop = do
    cmd <- request ()
    (reply, stop) <- case cmd of
        MultiBulk ["PING"] ->
            return (Status "PONG", False)
        MultiBulk ["QUIT"] ->
            return (Status "OK", True)
        MultiBulk ["SET", Bulk key, Bulk value] -> do
            _ <- lift $ set store (lazy key) (lazy value)
            return (Status "OK", True)
        _ ->
            return (Error "ERR unknown command", False)
    respond reply
    unless stop loop

-- | Make a strict 'ByteString' lazy.
lazy :: ByteString -> BL.ByteString
lazy s = BL.fromChunks [s]
