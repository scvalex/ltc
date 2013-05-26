{-# LANGUAGE DeriveGeneric #-}

module Ltc.Store.Event (
        -- * Types
        EventHandler(..), Event(..)
    ) where

import GHC.Generics ( Generic )
import Ltc.Store.Types ( Key )

-- | An event signals that something has happened inside an "Ltc.Store".
data Event = SetEvent Key
           | GetEvent Key
           | CloseEvent
           deriving ( Show, Generic )

-- | An 'EventHandler' is something that consumes 'Event's.  This is just the type-class;
-- see the other modules for actual event handlers.
class EventHandler a where
    -- | Handle an event.  All event handlers associated with a store are run on a
    -- separate thread, so this should not block, and it should not throw exceptions.
    handleEvent :: a -> Event -> IO ()
