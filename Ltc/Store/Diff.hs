{-# LANGUAGE GADTs, DeriveDataTypeable, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ltc.Store.Diff (
        Diffable(..), Diff(..)
    ) where

import Data.Set ( Set )
import Ltc.Store.Class ( Value(..), Single, Collection )
import qualified Data.Set as S

data Diff a where
    DiffInt :: Integer -> Diff (Single Integer)
    -- FIXME Diffs for strings
    DiffSet :: Set (Value (Single b)) -> Set (Value (Single b)) -> Diff (Collection b)

class Diffable a where
    diffFromTo :: Value a -> Value a -> Diff a
    applyDiff :: Value a -> Diff a -> Value a
    reverseDiff :: Diff a -> Diff a

instance Diffable (Single Integer) where
    diffFromTo (VaInt n1) (VaInt n2) = DiffInt (n2 - n1)

    applyDiff (VaInt n) (DiffInt d) = VaInt (n + d)

    reverseDiff (DiffInt d) = DiffInt (-d)

instance (Ord (Value (Single b))) => Diffable (Collection b) where
    diffFromTo (VaSet s1) (VaSet s2) =
        DiffSet (S.difference s1 s2) (S.difference s2 s1)

    applyDiff (VaSet s) (DiffSet toRemove toAdd) =
        VaSet (S.union (S.difference s toRemove) toAdd)

    reverseDiff (DiffSet toRemove toAdd) = DiffSet toAdd toRemove
