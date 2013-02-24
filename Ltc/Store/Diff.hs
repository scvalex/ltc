{-# LANGUAGE GADTs, DeriveDataTypeable, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ltc.Store.Diff (
        Diffable(..), Diff
    ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Set ( Set )
import Data.Sexp ( Sexpable(..), Sexp(..) )
import Ltc.Store.Class ( Value(..), Single, Collection )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S

import Data.Array
import Data.List

data Diff a where
    DiffInt :: Integer -> Diff (Single Integer)
    -- FIXME Diffs for strings
    DiffSet :: Set (Value (Single b)) -> Set (Value (Single b)) -> Diff (Collection b)

instance Sexpable (Diff (Single Integer)) where
    toSexp (DiffInt n) = List ["DiffInt", toSexp n]
    fromSexp (List ["DiffInt", s]) = DiffInt <$> fromSexp s
    fromSexp _                     = fail "fromSexp Diff (Single Integer)"

instance Sexpable (Diff (Collection Integer)) where
    toSexp (DiffSet toRemove toAdd) = List ["DiffSet", toSexp toRemove, toSexp toAdd]

    fromSexp (List ["DiffSet", s1, s2]) = DiffSet <$> fromSexp s1 <*> fromSexp s2
    fromSexp _                          = fail "fromSexp Diff (Collection Integer)"

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

--------------------------------
-- ByteString diffs
--------------------------------

-- Adapted from the Diff package.

-- | A difference indicator is a value from the /F/irst list, from the /S/econd one, or
-- from /B/oth.
data DI = F | S | B
        deriving ( Show, Eq )

data DL = DL { poi  :: !Int
             , poj  :: !Int
             , path :: [DI]
             }
        deriving ( Show, Eq )

instance Ord DL where
    x <= y = poi x <= poi y

-- | Return a list indicating the differences between the two given lists.
getDiff :: (Eq a) => [a] -> [a] -> [(DI, a)]
getDiff a b = markup a b (reverse (lcs a b))
  where
    markup (x:xs) ys     (F:ds) = (F, x) : markup xs ys ds
    markup xs     (y:ys) (S:ds) = (S, y) : markup xs ys ds
    markup (x:xs) (_:ys) (B:ds) = (B, x) : markup xs ys ds
    markup _      _      _      = []

-- | Takes two lists and returns a list indicating the differences
-- between them, grouped into chunks.
getGroupedDiff :: (Eq a) => [a] -> [a] -> [(DI, [a])]
getGroupedDiff as bs = map go (groupBy (\x y -> fst x == fst y) (getDiff as bs))
  where
    go ((d, x) : xs) = (d, x : map snd xs)

-- | Compute the longest common subsequence with the usual matrix method.
lcs :: (Eq a) => [a] -> [a] -> [DI]
lcs as bs =
    let initPath = addSnake cD (DL { poi = 0, poj = 0, path = []})
        allPaths = concat (iterate (dstep cD) [initPath])
        fullPaths = dropWhile (\dl -> poi dl /= lena || poj dl /= lenb) allPaths
    in path (head fullPaths)
  where
    cD = canDiag as bs lena lenb
    lena = length as
    lenb = length bs

-- FIXME Why does dstep pick out the first element?
-- | Expand all paths one step (and then expand them across the diagonals).
dstep :: (Int -> Int -> Bool) -> [DL] -> [DL]
dstep cD dls = hd : pairMaxes rst
  where
    (hd:rst) = nextDLs dls

    -- Replace each path, by the path starting one beneath and one to the right (expanded
    -- down the diagonal).
    nextDLs :: [DL] -> [DL]
    nextDLs [] = []
    nextDLs (dl:rest) = dl' : dl'' : nextDLs rest
      where
        dl'  = addSnake cD $ dl { poi = poi dl + 1, path = (F : pdl) }
        dl'' = addSnake cD $ dl { poj = poj dl + 1, path = (S : pdl) }
        pdl  = path dl

    -- Pick the maximum path in each pair of given paths.
    pairMaxes :: [DL] -> [DL]
    pairMaxes []         = []
    pairMaxes [x]        = [x]
    pairMaxes (x:y:rest) = max x y : pairMaxes rest

-- | Advance the given path along the diagonal as much as possible.
addSnake :: (Int -> Int -> Bool) -> DL -> DL
addSnake cD dl
    | cD pi pj = addSnake cD $
                 dl { poi = pi + 1, poj = pj + 1, path = (B : path dl) }
    | otherwise = dl
  where
    pi = poi dl
    pj = poj dl

-- | Can we move one step along the diagonal?
canDiag :: (Eq a) => [a] -> [a] -> Int -> Int -> Int -> Int -> Bool
canDiag as bs lena lenb i j =
    i < lena && j < lenb && (arAs ! i) == (arBs ! j)
  where
    arAs = listArray (0, lena - 1) as
    arBs = listArray (0, lenb - 1) bs
