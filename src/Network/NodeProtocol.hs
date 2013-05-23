{-# LANGUAGE DeriveGeneric #-}

module Network.NodeProtocol (
        NodeEnvelope(..), NodeMessage(..), encode, decode
    ) where

import Data.ByteString ( ByteString )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable )
import Network.Types ( Hostname, Port )
import qualified Data.Serialize as S

----------------------
-- Message type
----------------------

data NodeEnvelope = NodeEnvelope
    { getEnvelopeSender  :: (Hostname, Port)
    , getEnvelopeMessage :: NodeMessage
    } deriving ( Generic, Show )

instance Serialize NodeEnvelope

instance Sexpable NodeEnvelope

data NodeMessage = Ping String
                 | Patch String
                 deriving ( Generic, Show )

instance Serialize NodeMessage

instance Sexpable NodeMessage

----------------------
-- (De)Serialization
----------------------

-- | Encode a 'NodeEnvelope' as a strict 'ByteString'.
encode :: NodeEnvelope -> ByteString
encode = S.encode

-- | Try to decode a 'NodeEnvelope'.
decode :: ByteString -> Maybe NodeEnvelope
decode bin =
    case S.decode bin of
        Left _  -> Nothing
        Right e -> Just e
