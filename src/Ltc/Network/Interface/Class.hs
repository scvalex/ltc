{-# LANGUAGE TypeFamilies, FlexibleContexts, EmptyDataDecls, MultiParamTypeClasses #-}

module Ltc.Network.Interface.Class (
        NetworkInterface(..), Sending, Receiving
    ) where

import Data.ByteString ( ByteString )
import Data.Serialize ( Serialize )
import Language.Sexp ( Sexpable )

-- | Used to tag a 'NetworkInterface' that can 'send'.
data Sending

-- | Used to tag a 'NetworkInterface' that can 'receive'.
data Receiving

-- | An abstract network interface.
class (Eq (NetworkLocation a),
       Ord (NetworkLocation a),
       Serialize (NetworkLocation a),
       Sexpable (NetworkLocation a),
       Show (NetworkLocation a)) => NetworkInterface a where
    data NetworkLocation a :: *

    -- | Create a receiving interface on the given location.
    serve :: NetworkLocation a -> IO (a Receiving)

    -- | Receive a single unitary message.
    receive :: a Receiving -> IO ByteString

    -- | Create a sending interface to the given location.
    connect :: NetworkLocation a -> IO (a Sending)

    -- | Send a single unitary message to the given location.
    send :: a Sending -> ByteString -> IO ()

    -- | Close an interface.  This is used for both sending and receiving interfaces.
    close :: a b -> IO ()
