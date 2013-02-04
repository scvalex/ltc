{-# LANGUAGE TypeFamilies, DeriveDataTypeable #-}

module Ltc.Store.Class (
        -- * Store interface
        Store(..),

        -- * Common types
        Key, KeyHash, ValueHash, Version, NodeName,

        -- * Value types
        Type(..), Value(..), valueString
    ) where

import Data.ByteString.Lazy.Char8 ( ByteString, pack )
import Data.Data ( Data, Typeable )
import Data.Set ( Set )
import Data.String ( IsString(..) )
import Data.VectorClock ( VectorClock )

type Key       = ByteString
type KeyHash   = ByteString
type ValueHash = ByteString
type NodeName  = ByteString
type Version   = VectorClock NodeName Int

-- | The type of a value stored in the store.
data Type = TyString
          | TyInt
          deriving ( Data, Show, Typeable )

data Value = VaString ByteString
           deriving ( Eq, Show )

instance IsString Value where
    fromString = VaString . pack

class Store a where
    data OpenParameters a :: *

    open :: OpenParameters a -> IO a
    close :: a -> IO ()

    storeFormat :: a -> String
    storeVersion :: a -> Int

    get :: a -> Key -> Version -> IO (Maybe Value)
    getLatest :: a -> Key -> IO (Maybe (Value, Version))

    keyVersions :: a -> Key -> IO (Maybe [Version])

    set :: a -> Key -> Value -> IO Version

    keys :: a -> IO (Set Key)

-- | Get the 'ByteString' representation of a value.
valueString :: Value -> ByteString
valueString (VaString s) = s
