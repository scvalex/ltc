{-# LANGUAGE DeriveGeneric #-}

module Ltc.Store.Event (
        -- * Types
        Event(..), EventChannel
    ) where

import Control.Concurrent.STM.TChan ( TChan )
import GHC.Generics ( Generic )
import Ltc.Store.Types ( Key )

-- | An event signals that something has happened inside an "Ltc.Store".
data Event = SetEvent Key
           | GetEvent Key
           | CloseEvent
           deriving ( Show, Generic )

type EventChannel = TChan Event
