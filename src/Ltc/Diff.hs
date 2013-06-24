{-# LANGUAGE DeriveGeneric, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides the types and functions for working with changes to values.  See
-- the documentation for 'Diffable' for more information.
module Ltc.Diff (
        Diffable(..),

        -- * Internal types (useful for testing)
        EditScript, EditAction
    ) where

import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Default ( Default(..) )
import Data.List ( groupBy )
import Data.Serialize ( Serialize(..) )
import Data.Set ( Set )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable(..) )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S

--------------------------------
-- Diffable
--------------------------------

-- | 'Diffable' is a way of creating, manipulating, and applying diffs.  The diffs created
-- here have the properties mentioned in the documentation for the class functions.
class (Default a, Eq (Diff a), Show (Diff a), Sexpable (Diff a), Serialize (Diff a))
      => Diffable a where
    data Diff a :: *

    -- | Generate the 'Diff' from the first value to the second.  In other words, this is
    -- the 'Diff' that applied to the first value, yields the second.
    --
    -- @
    --     applyDiff v1 (diffFromTo v1 v2) == v2
    -- @
    diffFromTo :: a -> a -> Diff a

    -- | Apply the given 'Diff' to the given value.  Note that applying a 'Diff' to a
    -- value other than the one it was generated from may give unexpected results.
    applyDiff :: a -> Diff a -> a

    -- | Reverse a 'Diff'.  In other words, if you have a 'Diff' that turns one value into
    -- another, its reverse is the 'Diff' that turns the second value into the first.
    --
    -- @
    --     applyDiff v2 (reverseDiff (diffFromTo v1 v2)) == v1
    -- @
    reverseDiff :: Diff a -> Diff a

    -- | Merge two 'Diff's.  This operation should be commutative.
    mergeDiffs :: Diff a -> Diff a -> Diff a

--------------------------------
-- Diffable Integer
--------------------------------

instance Diffable Integer where
    data Diff Integer = DiffInt Integer

    diffFromTo n1 n2 = DiffInt (n2 - n1)

    applyDiff n (DiffInt d) = n + d

    reverseDiff (DiffInt d) = DiffInt (-d)

    mergeDiffs (DiffInt a) (DiffInt b) = DiffInt (a + b)

deriving instance Eq (Diff Integer)

deriving instance Show (Diff Integer)

deriving instance Generic (Diff Integer)

instance Serialize (Diff Integer)

instance Sexpable (Diff Integer)

--------------------------------
-- Diffable ByteString
--------------------------------

instance Diffable ByteString where
    data Diff ByteString = DiffString EditScript

    diffFromTo s1 s2 = DiffString (editsFromTo s1 s2)

    applyDiff s (DiffString d) = applyEdits s d

    reverseDiff (DiffString d) = DiffString (reverseEdits d)

    mergeDiffs (DiffString _a) (DiffString _b) = error "cannot merge string diffs"

deriving instance Eq (Diff ByteString)

deriving instance Show (Diff ByteString)

deriving instance Generic (Diff ByteString)

instance Serialize (Diff ByteString)

instance Sexpable (Diff ByteString)

instance Default ByteString where
    def = ""

--------------------------------
-- Diffable Set
--------------------------------

instance (Ord a, Show a, Serialize a, Sexpable a) => Diffable (Set a) where
    data Diff (Set a) = DiffSet (Set a) (Set a)

    diffFromTo s1 s2 =
        DiffSet (S.difference s1 s2) (S.difference s2 s1)

    applyDiff s (DiffSet toRemove toAdd) =
        S.union (S.difference s toRemove) toAdd

    reverseDiff (DiffSet toRemove toAdd) =
        DiffSet toAdd toRemove

    mergeDiffs (DiffSet _toRemove1 _toAdd1) (DiffSet _toRemove2 _toAdd2) =
        error "cannot merge set diffs"

deriving instance (Eq a) => Eq (Diff (Set a))

deriving instance (Show a) => Show (Diff (Set a))

deriving instance Generic (Diff (Set a))

instance (Serialize a, Ord a) => Serialize (Diff (Set a))

instance (Sexpable a, Ord a) => Sexpable (Diff (Set a))

--------------------------------
-- Edit scripts
--------------------------------

-- | An 'EditScript' defines the actions that change one 'ByteString' into another.
newtype EditScript = EditScript [EditAction]
                   deriving ( Eq, Generic, Show )

instance Serialize EditScript

instance Sexpable EditScript

data EditAction = Skip Int
                | Insert ByteString
                | Delete ByteString
                deriving ( Eq, Generic, Show )

instance Serialize EditAction

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

data DL = DL
    { poi  :: !Int
    , poj  :: !Int
    , path :: [DI]
    } deriving ( Show, Eq )

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
