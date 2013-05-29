{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Network.Interface (
        NetworkInterface(..)
    ) where

import Data.ByteString ( ByteString )

class (Show (NetworkLocation a)) => NetworkInterface a where
    data NetworkLocation a :: *

    serve :: NetworkLocation a -> IO a
    receive :: a -> IO ByteString

    connect :: NetworkLocation a -> IO a
    send :: a -> ByteString -> IO ()

    close :: a -> IO ()
