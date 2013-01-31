{-# LANGUAGE TypeFamilies #-}

module Ltc.Store.Class (
        Store(..), Key, KeyHash, Value, ValueHash, Version, NodeName
    ) where

import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Set ( Set )
import Data.VectorClock ( VectorClock )

type Key       = ByteString
type KeyHash   = ByteString
type Value     = ByteString
type ValueHash = ByteString
type NodeName  = ByteString
type Version   = VectorClock NodeName Int

class Store a where
    data OpenParameters a :: *

    open :: OpenParameters a -> IO a
    close :: a -> IO ()

    get :: a -> Key -> Version -> IO (Maybe Value)
    getLatest :: a -> Key -> IO (Maybe (Value, Version))

    keyVersions :: a -> Key -> IO (Maybe [Version])

    set :: a -> Key -> Value -> IO Version

    keys :: a -> IO (Set Key)
