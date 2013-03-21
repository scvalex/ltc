{-# LANGUAGE DeriveGeneric #-}

module Network.NodeProtocol (
        NodeMessage, encode, decode
    ) where

import Data.ByteString.Char8 ( ByteString )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import qualified Data.Serialize as S

----------------------
-- Message type
----------------------

data NodeMessage = Ping String
                 deriving ( Generic )

instance Serialize NodeMessage

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
