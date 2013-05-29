{-# LANGUAGE TypeFamilies #-}

module Network.Interface (
        NetworkInterface(..)
    ) where

import Data.ByteString ( ByteString )

class NetworkInterface a where
    data Location a :: *

    serve :: Location a -> IO a
    receive :: a -> IO ByteString

    connect :: Location a -> IO a
    send :: a -> ByteString -> IO ()
