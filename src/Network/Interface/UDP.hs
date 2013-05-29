{-# LANGUAGE TypeFamilies #-}

module Network.Interface.UDP (
        UDPInterface, NetworkLocation(..)
    ) where

import Network.Socket ( Socket(..), socket, sClose, bindSocket, iNADDR_ANY
                      , AddrInfo(..), getAddrInfo, defaultHints
                      , Family(..), SocketType(..), SockAddr(..)
                      , SocketOption(..), setSocketOption, defaultProtocol )
import Network.Socket.ByteString ( sendAll, recv )
import Network.Types ( Hostname, Port )
import Network.Interface ( NetworkInterface(..) )
import qualified Control.Exception as CE
import qualified Network.Socket as NS

-- FIXME Use phantoms to distinguish between sending/receiving sockets.
data UDPInterface = UDPInterface
    { getSocket :: Socket
    }

instance NetworkInterface UDPInterface where
    data NetworkLocation UDPInterface = NetworkLocation
        { host :: Hostname
        , port :: Port
        } deriving ( Show )

    serve addr = do
        CE.bracketOnError
            (socket AF_INET Datagram defaultProtocol)
            sClose
            (\sock -> do
                  -- FIXME See the examples at the end of Network.Socket.ByteString
                  setSocketOption sock ReuseAddr 1
                  bindSocket sock (SockAddrInet (fromIntegral (port addr)) iNADDR_ANY)
                  return $ UDPInterface { getSocket = sock })

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
                 return $ UDPInterface { getSocket = sock})

    send intf bin = sendAll (getSocket intf) bin

    close intf = sClose (getSocket intf)
