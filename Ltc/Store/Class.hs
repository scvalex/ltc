{-# LANGUAGE TypeFamilies #-}

module Ltc.Store.Class (
        Store(..), Key, Value, Version
    ) where

import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.VectorClock ( VectorClock )

type Key = String
type Value = ByteString
type Version = VectorClock ByteString Int

class Store a where
    data ConnectParameters a :: *

    open :: ConnectParameters a -> IO a
    close :: a -> IO ()

    get :: a -> Key -> Version -> IO (Maybe Value)
    getLatest :: a -> Key -> IO (Maybe (Value, Version))

    set :: a -> Key -> Value -> IO Version

    del :: a -> Key -> IO (Maybe Version)
