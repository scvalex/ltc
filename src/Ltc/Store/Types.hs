{-# LANGUAGE DeriveGeneric, FlexibleInstances, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ltc.Store.Types (
        -- * Key Types
        Key(..), KeyHash,

        -- * Storables
        Storable, ValueHash,
        Type, typeOf,

        -- * Changes
        ChangesetHash,

        -- * Node types
        NodeName,

        -- * Version types
        Version
    ) where

import Data.Aeson ( ToJSON )
import Data.ByteString.Lazy.Char8 ( ByteString, pack )
import Data.Serialize ( Serialize(..) )
import Data.String ( IsString(..) )
import Data.Set ( Set )
import Data.Typeable ( Typeable(..) )
import Data.Typeable.Internal ( TypeRep(..), TyCon(..), Fingerprint(..) )
import Data.VectorClock ( VectorClock )
import GHC.Generics ( Generic )
import GHC.Word ( Word64 )
import Language.Sexp ( Sexpable(..) )
import Ltc.Diff ( Diffable )

----------------------
-- LTc types and instances
----------------------

newtype Key = Key ByteString
            deriving ( Eq, Generic, Ord, Show )

instance Sexpable Key

instance ToJSON Key

instance Serialize Key

instance IsString Key where
    fromString = Key . pack

-- FIXME Make KeyHash, ValueHjash, NodeName, and Version abstract types
type KeyHash = ByteString
type ValueHash = ByteString
type ChangesetHash = ByteString
type NodeName = ByteString

type Version = VectorClock NodeName Int

instance Serialize Version

instance Sexpable Version

class (Eq a, Ord a, Serialize a, Sexpable a, Typeable a, Diffable a)
      => Storable a

instance Storable Integer

instance Storable ByteString

instance Storable (Set Integer)

instance Storable (Set ByteString)

----------------------
-- Wrapper around TypeRep (we need it to be serializable)
----------------------

type Type = TypeRep

deriving instance Generic Type

instance Sexpable Type

instance Serialize Type

deriving instance Generic TyCon

instance Sexpable TyCon

instance Serialize TyCon

deriving instance Generic Fingerprint

instance Sexpable Fingerprint

instance Serialize Fingerprint

instance Sexpable Word64 where
    toSexp = toSexp . toInteger

    -- FIXME Why can't I η-reduce this?
    fromSexp s = return . fromInteger =<< fromSexp s
