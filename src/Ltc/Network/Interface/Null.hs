{-# LANGUAGE TypeFamilies, FlexibleInstances, DeriveGeneric #-}

module Ltc.Network.Interface.Null (
        NullInterface, NetworkLocation(..)
    ) where

import Control.Concurrent ( newEmptyMVar, takeMVar )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable )
import Ltc.Network.Interface.Class ( NetworkInterface(..) )

-- | The null interface discards any message sent and blocks indefinitely on receive.
data NullInterface a = NullInterface

instance NetworkInterface NullInterface where
    -- | A null location is just that.
    data NetworkLocation NullInterface = NullLocation
                                       deriving ( Eq, Generic, Ord, Show )

    serve _ = return NullInterface
    receive _ = takeMVar =<< newEmptyMVar

    connect _ = return NullInterface
    send _ _ = return ()

    close _ = return ()

instance Serialize (NetworkLocation NullInterface)

instance Sexpable (NetworkLocation NullInterface)
