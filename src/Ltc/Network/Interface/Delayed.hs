{-# LANGUAGE TypeFamilies, FlexibleInstances, DeriveGeneric #-}

module Ltc.Network.Interface.Delayed (
        DelayedInterface, NetworkLocation(..)
    ) where

import Control.Applicative ( (<$>) )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable )
import Ltc.Network.Interface.Class ( NetworkInterface(..) )
import Ltc.Network.Interface.UDP ( UdpInterface, NetworkLocation(..) )
import Ltc.Network.Types ( Hostname, Port )
import Text.Printf ( printf )

----------------------
-- NetworkInterface instance
----------------------

-- | The delayed interface introduces random, configurable delays on sends.  It is based
-- on 'UdpInterface'.
data DelayedInterface = DelayedInterface
    { getUdpInterface :: UdpInterface
    }

instance NetworkInterface DelayedInterface where
    -- | A delayed location is a host/port pair.
    data NetworkLocation DelayedInterface = DelayedLocation Hostname Port
                                          deriving ( Eq, Generic, Ord )

    serve location = DelayedInterface <$> serve (delayedToUdpLocation location)
    receive intf = receive (getUdpInterface intf)

    connect location = DelayedInterface <$> connect (delayedToUdpLocation location)
    send intf = send (getUdpInterface intf)

    close intf = close (getUdpInterface intf)

instance Show (NetworkLocation DelayedInterface) where
    show (DelayedLocation h p) = printf "%s:%d" h p

instance Serialize (NetworkLocation DelayedInterface)

instance Sexpable (NetworkLocation DelayedInterface)

----------------------
-- Helpers
----------------------

-- | Convert a 'DelayedLocation' to a 'UdpLocation'.
delayedToUdpLocation :: NetworkLocation DelayedInterface -> NetworkLocation UdpInterface
delayedToUdpLocation (DelayedLocation h p) = UdpLocation { host = h, port = p }
