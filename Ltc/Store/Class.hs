{-# LANGUAGE TypeFamilies #-}

module Ltc.Store.Class (
        Store(..), Key, Value, ValueHash, Version
    ) where

import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.VectorClock ( VectorClock )

type Key       = ByteString
type Value     = ByteString
type ValueHash = ByteString
type Version   = VectorClock ByteString Int

class Store a where
    data OpenParameters a :: *

    open :: OpenParameters a -> IO a
    close :: a -> IO ()

    get :: a -> Key -> Version -> IO (Maybe Value)
    getLatest :: a -> Key -> IO (Maybe (Value, Version))

    set :: a -> Key -> Value -> IO Version

    del :: a -> Key -> IO (Maybe Version)
