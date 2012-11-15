{-# LANGUAGE TypeFamilies #-}

module Ltc.Store.Class (
        Store(..)
    ) where

import Data.ByteString ( ByteString )

type Key = String
type Value = ByteString
type Version = Int

class Store a where
    data ConnectParameters :: *

    connect :: ConnectParameters -> IO (Maybe a)
    close :: a -> IO (Maybe ())

    get :: a -> Key -> Version -> IO (Maybe Value)
    getLatest :: a -> Key -> IO (Maybe (Value, Version))

    set :: a -> Key -> Value -> IO (Maybe Version)

    del :: a -> Key -> IO (Maybe Version)
