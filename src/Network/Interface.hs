{-# LANGUAGE TypeFamilies #-}

module Network.Interface (
        NetworkInterface(..)
    ) where

import Data.ByteString ( ByteString )

class NetworkInterface a where
    data NetworkLocation a :: *

    serve :: NetworkLocation a -> IO a
    receive :: a -> IO ByteString

    connect :: NetworkLocation a -> IO a
    send :: a -> ByteString -> IO ()

    close :: a -> IO ()
