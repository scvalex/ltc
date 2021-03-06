{-# LANGUAGE TypeFamilies, FlexibleInstances, DeriveGeneric #-}

-- | The UDP interface exposes the operating system's underlying UDP network functions as
-- a 'NetworkInterface', and handles name resolution automatically.
module Ltc.Network.Interface.UDP (
        UdpInterface, NetworkLocation(..)
    ) where

import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable )
import Ltc.Network.Interface.Class ( NetworkInterface(..) )
import Ltc.Network.Types ( Hostname, Port )
import Network.Socket ( Socket(..), socket, sClose, bindSocket, iNADDR_ANY
                      , AddrInfo(..), getAddrInfo, defaultHints
                      , Family(..), SocketType(..), SockAddr(..)
                      , SocketOption(..), setSocketOption, defaultProtocol )
import Network.Socket.ByteString ( sendAll, recv )
import qualified Control.Exception as CE
import qualified Network.Socket as NS
import System.Log.Logger ( debugM )
import Text.Printf ( printf )

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "UDP"

----------------------
-- UDP Interface
----------------------

-- | The abstract type of a UDP socket.  This is used both for receiving and sending
-- sockets.
data UdpInterface a = UdpInterface
    { getSocket :: Socket
    }

instance NetworkInterface UdpInterface where
    -- | A UDP network location is a host/port pair.
    data NetworkLocation UdpInterface = UdpLocation
        { host :: Hostname
        , port :: Port
        } deriving ( Eq, Generic, Ord )

    serve addr = do
        CE.bracketOnError
            (socket AF_INET Datagram defaultProtocol)
            sClose
            (\sock -> do
                  -- FIXME See the examples at the end of Network.Socket.ByteString
                  setSocketOption sock ReuseAddr 1
                  bindSocket sock (SockAddrInet (fromIntegral (port addr)) iNADDR_ANY)
                  return $ UdpInterface { getSocket = sock })

    receive intf = recv (getSocket intf) 4096

    connect addr = do
        -- FIXME Getting the head of addrInfos seems brittle.
        addrInfos <- getAddrInfo (Just (defaultHints { addrFamily = AF_INET }))
                                 (Just (host addr))
                                 (Just $ show (port addr))
        CE.bracketOnError
            (socket AF_INET Datagram defaultProtocol)
            sClose
            (\sock -> do
                 NS.connect sock (addrAddress $ head addrInfos)
                 return $ UdpInterface { getSocket = sock})

    send intf bin =
        CE.handle (\(_ :: CE.SomeException) -> debugM tag "UDP failed to send")
            (sendAll (getSocket intf) bin)

    close intf = sClose (getSocket intf)

instance Show (NetworkLocation UdpInterface) where
    show location = printf "%s:%d" (host location) (port location)

instance Serialize (NetworkLocation UdpInterface)

instance Sexpable (NetworkLocation UdpInterface)
