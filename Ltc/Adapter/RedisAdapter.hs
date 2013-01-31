module Ltc.Adapter.RedisAdapter (
        redisProxyD
    ) where

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
        MultiBulk [Bulk "PING"] ->
            return (Status "PONG", False)
        MultiBulk [Bulk "QUIT"] ->
            return (Status "OK", True)
        _ ->
            return (Error "ERR unknown command", False)
    respond reply
    unless stop loop
