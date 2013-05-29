{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Network.Interface (
        NetworkInterface(..)
    ) where

import Data.ByteString ( ByteString )
import Data.Serialize ( Serialize )
import Language.Sexp ( Sexpable )

-- | An abstract network interface.
class (Eq (NetworkLocation a),
       Ord (NetworkLocation a),
       Serialize (NetworkLocation a),
       Sexpable (NetworkLocation a),
       Show (NetworkLocation a)) => NetworkInterface a where
    data NetworkLocation a :: *

    -- | Create a receiving interface on the given location.
    serve :: NetworkLocation a -> IO a

    -- | Receive a single unitary message.
    receive :: a -> IO ByteString

    -- | Create a sending interface to the given location.
    connect :: NetworkLocation a -> IO a

    -- | Send a single unitary message to the given location.
    send :: a -> ByteString -> IO ()

    -- | Close an interface.  This is used for both sending and receiving interfaces.
    close :: a -> IO ()
