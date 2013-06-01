{-# LANGUAGE DeriveGeneric #-}

module Ltc.Store.Event (
        -- * Types
        Event(..), SetEvent(..), EventChannel
    ) where

import Control.Concurrent.STM.TChan ( TChan )
import Data.Aeson ( ToJSON )
import GHC.Generics ( Generic )
import Ltc.Store.Types ( Key )

-- | An event signals that something has happened inside an "Ltc.Store".
data Event = MSetEvent [SetEvent]
           | GetEvent { eventKey :: Key, keyDigest :: Int }
           | CloseEvent
           deriving ( Show, Generic )

instance ToJSON Event

-- | A 'SetEvent' records that the value of a key was changed.
data SetEvent = SetEvent { setKey :: Key, setKeyDigest :: Int, valueDigest :: Int }
                deriving ( Show, Generic )

instance ToJSON SetEvent

type EventChannel = TChan Event
