{-# LANGUAGE GADTs, DeriveDataTypeable, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ltc.Store.Diff (
        Diffable(..), Diff(..), SetOperation(..)
    ) where

import Data.Data ( Data, Typeable )
import Data.Set ( Set )
import Ltc.Store.Class ( Value(..), Single, Collection )
import qualified Data.Set as S

-- | Are we adding to, or subtracting elements from a set?
data SetOperation = AddElements
                  | RemoveElements
                    deriving ( Data, Eq, Show, Typeable )

data Diff a where
    DiffInt :: Integer -> Diff (Single Integer)
    -- FIXME Diffs for strings
    DiffSet :: SetOperation -> Set (Value (Single b)) -> Diff (Collection b)

class Diffable a where
    diffFromTo :: Value a -> Value a -> Diff a
    applyDiff :: Value a -> Diff a -> Value a

instance Diffable (Single Integer) where
    diffFromTo (VaInt n1) (VaInt n2) = DiffInt (n2 - n1)

    applyDiff (VaInt n) (DiffInt d) = VaInt (n + d)

instance (Ord (Value (Single b))) => Diffable (Collection b) where
    diffFromTo (VaSet s1) (VaSet s2) =
        if S.size s1 > S.size s2
        then DiffSet RemoveElements (S.difference s1 s2)
        else DiffSet AddElements (S.difference s2 s1)

    applyDiff (VaSet s) (DiffSet AddElements s') =
        VaSet (S.union s s')
    applyDiff (VaSet s) (DiffSet RemoveElements s') =
        VaSet (S.difference s s')
