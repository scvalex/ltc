module Ltc.Store.EventHandler (
        -- * Types
        EventHandler(..), Event(..)
    ) where

-- | An event signals that something has happened inside an "Ltc.Store".
data Event = GenericEvent String

-- | An 'EventHandler' is something that consumes 'Event's.  This is just the type-class;
-- see the other modules for actual event handlers.
class EventHandler a where
    handleEvent :: a -> Event -> IO ()
