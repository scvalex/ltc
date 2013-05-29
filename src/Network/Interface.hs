{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Network.Interface (
        NetworkInterface(..)
    ) where

import Data.ByteString ( ByteString )
import Data.Serialize ( Serialize )
import Language.Sexp ( Sexpable )

class (Show (NetworkLocation a),
       Serialize (NetworkLocation a),
       Sexpable (NetworkLocation a)) => NetworkInterface a where
    data NetworkLocation a :: *

    serve :: NetworkLocation a -> IO a
    receive :: a -> IO ByteString

    connect :: NetworkLocation a -> IO a
    send :: a -> ByteString -> IO ()

    close :: a -> IO ()
