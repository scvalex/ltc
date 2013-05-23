{-# LANGUAGE DeriveGeneric #-}

module Network.NodeProtocol (
        NodeMessage(..), encode, decode
    ) where

import Data.ByteString ( ByteString )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable )
import qualified Data.Serialize as S

----------------------
-- Message type
----------------------

-- FIXME All messages should be put in an envelope which includes the sender address
-- (because the sending UDP port is not the same as the receiving one)
data NodeMessage = Ping String
                 | Patch String
                 deriving ( Generic, Show )

instance Serialize NodeMessage

instance Sexpable NodeMessage

----------------------
-- (De)Serialization
----------------------

-- | Encode a 'NodeMessage' as a strict 'ByteString'.
encode :: NodeMessage -> ByteString
encode = S.encode

-- | Try to decode a 'NodeMessage'.
decode :: ByteString -> Maybe NodeMessage
decode s =
    case S.decode s of
        Left _  -> Nothing
        Right m -> Just m
