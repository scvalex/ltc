{-# LANGUAGE GADTs, DeriveDataTypeable, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, DeriveGeneric #-}

module Ltc.Store.Diff (
        Diffable(..), Diff
    ) where

import Control.Applicative ( (<$>), (<*>) )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.List ( groupBy )
import Data.Set ( Set )
import Data.Sexp ( Sexpable(..), Sexp(..) )
import GHC.Generics ( Generic )
import Ltc.Store.Class ( Value(..), Single, Collection )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S

data Diff a where
    DiffInt :: Integer -> Diff (Single Integer)
    DiffString :: EditScript -> Diff (Single ByteString)
    DiffSet :: Set (Value (Single b)) -> Set (Value (Single b)) -> Diff (Collection b)

instance Sexpable (Diff (Single Integer)) where
    toSexp (DiffInt n) = List ["DiffInt", toSexp n]
    fromSexp (List ["DiffInt", s]) = DiffInt <$> fromSexp s
    fromSexp _                     = fail "fromSexp Diff (Single Integer)"

instance Sexpable (Diff (Single ByteString)) where
    toSexp (DiffString es) = List ["DiffString", toSexp es]
    fromSexp (List ["DiffString", s]) = DiffString <$> fromSexp s
    fromSexp _                        = fail "fromSexp Diff (Single ByteString)"

instance (Sexpable (Value (Single a)), Ord (Value (Single a)))
         => Sexpable (Diff (Collection a)) where
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

instance Diffable (Single ByteString) where
    diffFromTo (VaString s1) (VaString s2) = DiffString (editsFromTo s1 s2)

    applyDiff (VaString s) (DiffString d) = VaString (applyEdits s d)

    reverseDiff (DiffString d) = DiffString (reverseEdits d)

instance (Ord (Value (Single b))) => Diffable (Collection b) where
    diffFromTo (VaSet s1) (VaSet s2) =
        DiffSet (S.difference s1 s2) (S.difference s2 s1)

    applyDiff (VaSet s) (DiffSet toRemove toAdd) =
        VaSet (S.union (S.difference s toRemove) toAdd)

    reverseDiff (DiffSet toRemove toAdd) = DiffSet toAdd toRemove

--------------------------------
-- Edit scripts
--------------------------------

-- | An 'EditScript' defines the actions that change one 'ByteString' into another.
newtype EditScript = EditScript [EditAction]
                   deriving ( Generic, Show )

instance Sexpable EditScript

data EditAction = Skip Int
                | Insert ByteString
                | Delete ByteString
                deriving ( Generic, Show )

instance Sexpable EditAction

-- | Compute the edits that turn one 'ByteString' into another.
editsFromTo :: ByteString -> ByteString -> EditScript
editsFromTo s1 s2 = EditScript (map convert (diffByteString s1 s2))
  where
    convert (First, s)  = Delete s
    convert (Second, s) = Insert s
    convert (Both, s)   = Skip (fromIntegral (BL.length s))

-- | Change a 'ByteString' according to the given 'EditScript'.
applyEdits :: ByteString -> EditScript -> ByteString
applyEdits text (EditScript as) = fst (foldl applyAction (BL.empty, text) as)
  where
    applyAction (sofar, rest) (Delete s) =
        (sofar, BL.drop (BL.length s) rest)
    applyAction (sofar, rest) (Insert s) =
        (BL.append sofar s, rest)
    applyAction (sofar, rest) (Skip n) =
        let (common, rest') = BL.splitAt (fromIntegral n) rest in
        (BL.append sofar common, rest')

-- | Reverse an 'EditScript' such that it changes the destination 'ByteString' into the
-- original one.
reverseEdits :: EditScript -> EditScript
reverseEdits (EditScript as) = EditScript (map reverseAction as)
  where
    reverseAction (Skip n)   = Skip n
    reverseAction (Insert s) = Delete s
    reverseAction (Delete s) = Insert s

--------------------------------
-- ByteString diffs
--------------------------------

-- Adapted from the Diff package.

-- | A difference indicator is a value from the First list, from the Second one, or from
-- Both.
data DI = First | Second | Both
        deriving ( Show, Eq )

data DL = DL { poi  :: !Int
             , poj  :: !Int
             , path :: [DI]
             }
        deriving ( Show, Eq )

instance Ord DL where
    x <= y = poi x <= poi y

-- | Compute a list of the sub-'ByteString's that appear in each of the two given
-- 'ByteString's, or in both.
diffByteString :: ByteString -> ByteString -> [(DI, ByteString)]
diffByteString s1 s2 = map go (groupBy (\(x, _) (y, _) -> x == y) (getDiff s1 s2))
  where
    go :: [(DI, Char)] -> (DI, ByteString)
    go []            = error "groupBy returned an empty list"
    go ((d, c) : xs) = (d, BL.cons c (BL.pack (map snd xs)))

-- | Return a list indicating the differences between the two given lists.
getDiff :: ByteString -> ByteString -> [(DI, Char)]
getDiff s1 s2 = markup (BL.unpack s1) (BL.unpack s2) (reverse (lcs s1 s2))
  where
    markup (x:xs) ys     (First:ds)  = (First, x) : markup xs ys ds
    markup xs     (y:ys) (Second:ds) = (Second, y) : markup xs ys ds
    markup (x:xs) (_:ys) (Both:ds)   = (Both, x) : markup xs ys ds
    markup _      _      _           = []

-- | Compute the longest common subsequence with the usual matrix method.
lcs :: ByteString -> ByteString -> [DI]
lcs s1 s2 =
    let initPath = addSnake cD (DL { poi = 0, poj = 0, path = []})
        allPaths = concat (iterate (dstep cD) [initPath])
        fullPaths = dropWhile (\dl -> poi dl /= fromIntegral (BL.length s1) ||
                                      poj dl /= fromIntegral (BL.length s2)) allPaths
    in path (head fullPaths)
  where
    cD = canDiag s1 s2

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
        dl'  = addSnake cD $ dl { poi = poi dl + 1, path = (First : pdl) }
        dl'' = addSnake cD $ dl { poj = poj dl + 1, path = (Second : pdl) }
        pdl  = path dl

    -- Pick the maximum path in each pair of given paths.
    pairMaxes :: [DL] -> [DL]
    pairMaxes []         = []
    pairMaxes [x]        = [x]
    pairMaxes (x:y:rest) = max x y : pairMaxes rest

-- | Advance the given path along the diagonal as much as possible.
addSnake :: (Int -> Int -> Bool) -> DL -> DL
addSnake cD dl
    | cD posi posj = addSnake cD $
                     dl { poi = posi + 1, poj = posj + 1, path = (Both : path dl) }
    | otherwise = dl
  where
    posi = poi dl
    posj = poj dl

-- | Can we move one step along the diagonal?
canDiag :: ByteString -> ByteString -> Int -> Int -> Bool
canDiag s1 s2 i j =
    i < (fromIntegral (BL.length s1)) &&
    j < (fromIntegral (BL.length s2)) &&
    (BL.index s1 (fromIntegral i)) == (BL.index s2 (fromIntegral j))
