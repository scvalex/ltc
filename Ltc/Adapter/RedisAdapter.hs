module Ltc.Adapter.RedisAdapter (
        redisProxyD
    ) where

import Control.Proxy
import Ltc.Store
import Network.Redis

redisProxyD :: (Proxy p, Store s) => s -> () -> Pipe p RedisMessage RedisMessage IO ()
redisProxyD store () = runIdentityP $ forever $ do
    cmd <- request ()
    let reply = cmd
    lift $ print reply
    respond reply
