{-# LANGUAGE TypeFamilies, FlexibleInstances, DeriveGeneric #-}

module Ltc.Network.Interface.Delayed (
        DelayedInterface, NetworkLocation(..),

        -- * Delays
        setDelay, getDelay
    ) where

import Control.Concurrent ( MVar, newMVar, modifyMVar_, readMVar
                          , forkIO, threadDelay )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable )
import Ltc.Network.Interface.Class ( NetworkInterface(..) )
import Ltc.Network.Interface.UDP ( UdpInterface, NetworkLocation(..) )
import Ltc.Network.Types ( Hostname, Port )
import System.Random ( randomRIO )
import Text.Printf ( printf )

----------------------
-- NetworkInterface instance and accessors
----------------------

-- | The delayed interface introduces random, configurable delays on sends.  It is based
-- on 'UdpInterface'.
data DelayedInterface = DelayedInterface
    { getUdpInterface   :: UdpInterface
    , getArtificalDelay :: MVar (Double, Double)
    }

instance NetworkInterface DelayedInterface where
    -- | A delayed location is a host/port pair.
    data NetworkLocation DelayedInterface = DelayedLocation Hostname Port
                                          deriving ( Eq, Generic, Ord )

    serve location =
        delayedInterfaceWithDefaults
        =<< serve (delayedToUdpLocation location)

    receive intf = receive (getUdpInterface intf)

    connect location =
        delayedInterfaceWithDefaults
        =<< connect (delayedToUdpLocation location)

    send intf bin = do
        (minDelay, maxDelay) <- readMVar (getArtificalDelay intf)
        if maxDelay < 0.01
            then do
                send (getUdpInterface intf) bin
            else do
                d <- randomRIO (floor (1000000.0 * minDelay), floor (1000000.0 * maxDelay))
                _ <- forkIO $ do
                    threadDelay d
                    send (getUdpInterface intf) bin
                return ()

    close intf = close (getUdpInterface intf)

instance Show (NetworkLocation DelayedInterface) where
    show (DelayedLocation h p) = printf "%s:%d" h p

instance Serialize (NetworkLocation DelayedInterface)

instance Sexpable (NetworkLocation DelayedInterface)

-- | Set the artificial delay for the given interface.
setDelay :: DelayedInterface -> (Double, Double) -> IO ()
setDelay intf delay = modifyMVar_ (getArtificalDelay intf) (const (return delay))

-- | Get the artificial delay for the given interface.
getDelay :: DelayedInterface -> IO (Double, Double)
getDelay intf = readMVar (getArtificalDelay intf)

----------------------
-- Helpers
----------------------

-- | Convert a 'DelayedLocation' to a 'UdpLocation'.
delayedToUdpLocation :: NetworkLocation DelayedInterface -> NetworkLocation UdpInterface
delayedToUdpLocation (DelayedLocation h p) = UdpLocation { host = h, port = p }

-- | Make a 'DelayedInterface' from a 'UdpInterface'.
delayedInterfaceWithDefaults :: UdpInterface -> IO DelayedInterface
delayedInterfaceWithDefaults intf = do
    delay <- newMVar (0.0, 0.0)
    return (DelayedInterface { getUdpInterface   = intf
                             , getArtificalDelay = delay
                             })
