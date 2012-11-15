{-# LANGUAGE TypeFamilies #-}

module Ltc.Store (
        Store(..)
    ) where

import Data.ByteString ( ByteString )

type Key = String
type Value = ByteString
type Version = Int

class Store a where
    data Handle a :: *

    connect :: a -> IO (Maybe (Handle a))
    close :: Handle a -> IO (Maybe ())

    get :: Handle a -> Key -> Version -> IO (Maybe Value)
    getLatest :: Handle a -> Key -> IO (Maybe (Value, Version))
    set :: Handle a -> Key -> Value -> IO (Maybe Version)
    del :: Handle a -> Key -> IO (Maybe Version)
