module Ltc.Store.EventHandler (
        -- * Types
        EventHandler(..), Event(..)
    ) where

data Event = GenericEvent String

class EventHandler a where
    handleEvent :: a -> Event -> IO ()
