{-# LANGUAGE DeriveGeneric #-}

module Ltc.Store.Event (
        -- * Types
        Event(..), EventChannel
    ) where

import Control.Concurrent.STM.TChan ( TChan )
import Data.Aeson ( ToJSON )
import GHC.Generics ( Generic )
import Ltc.Store.Types ( Key )

-- | An event signals that something has happened inside an "Ltc.Store".
data Event = SetEvent Key
           | GetEvent Key
           | CloseEvent
           deriving ( Show, Generic )

instance ToJSON Event

type EventChannel = TChan Event
