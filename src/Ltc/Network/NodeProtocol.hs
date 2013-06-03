{-# LANGUAGE DeriveGeneric, ExistentialQuantification, StandaloneDeriving #-}

module Ltc.Network.NodeProtocol (
        NodeEnvelope(..), NodeMessage(..), encode, decode
    ) where

import Data.ByteString ( ByteString )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable )
import Ltc.Network.Interface ( NetworkInterface(..) )
import Ltc.Store ( Version )
import Ltc.Store.VersionControl ( DiffPack )
import qualified Data.Serialize as S

----------------------
-- Message type
----------------------

data NodeEnvelope a = NetworkInterface a => NodeEnvelope
    { getEnvelopeSender  :: NetworkLocation a
    , getEnvelopeMessage :: NodeMessage
    }

deriving instance Show (NodeEnvelope a)

-- | Type-parameter-less and versioned 'NodeEnvelope'.  This is what is actually sent on
-- the wire.
data SerializedNodeEnvelope
    = V0
    | V1 { getSender  :: ByteString
         , getMessage :: NodeMessage
         }
    deriving ( Generic, Show )

instance Serialize SerializedNodeEnvelope

instance Sexpable SerializedNodeEnvelope

data NodeMessage = Ping String
                 | Pong String
                 | Changes { getVersionClock :: Version
                           , getChanges      :: DiffPack
                           }
                 deriving ( Generic, Eq, Show )

instance Serialize NodeMessage

instance Sexpable NodeMessage

----------------------
-- (De)Serialization
----------------------

-- | Encode a 'NodeEnvelope' as a strict 'ByteString'.
encode :: (NetworkInterface a) => NodeEnvelope a -> ByteString
encode env =
    let serEnv = V1 { getSender  = S.encode (getEnvelopeSender env)
                    , getMessage = getEnvelopeMessage env
                    }
    in S.encode serEnv

-- | Try to decode a 'NodeEnvelope'.  Note that trying to decode a message with the wrong
-- network interface is undefined behaviour (it will probably fail).
decode :: (NetworkInterface a) => ByteString -> Maybe (NodeEnvelope a)
decode bin =
    case S.decode bin of
        Left _ ->
            Nothing
        Right V0 ->
            Nothing
        Right serEnv@(V1 {}) ->
            case S.decode (getSender serEnv) of
                Left _ ->
                    Nothing
                Right location ->
                    Just (NodeEnvelope { getEnvelopeSender  = location
                                       , getEnvelopeMessage = getMessage serEnv
                                       })
