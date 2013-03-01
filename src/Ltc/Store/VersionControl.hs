{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

module Ltc.Store.VersionControl (
       insertChangesInto
    ) where

import Data.Foldable ( foldlM )
import Ltc.Store.Class ( Store(..), Value, ValueString, ValueType, Key )
import Ltc.Store.Diff ( Diff, Diffable(..) )
import Ltc.Store.Serialization ( DiffPack(..), KeyHistory(..), getKeyHistory )
import qualified Data.Map as M

----------------------
-- Exposed interface
----------------------

-- | Insert the given changes into the store.  Returns a list of conflicting keys.
insertChangesInto :: (Store s) => s -> DiffPack -> IO [Key]
insertChangesInto store (DiffPack m) = foldlM insertKeyHistory [] (M.toList m)
  where
    insertKeyHistory :: [Key] -> (Key, KeyHistory) -> IO [Key]
    insertKeyHistory conflicts (key, theirHistory) = do
        mmyHistory <- getKeyHistory store key
        case tryMerge key mmyHistory theirHistory of
            Just acts  -> do
                mapM_ (applyAction store) acts
                return conflicts
            Nothing -> do
                return (key:conflicts)

----------------------
-- Store actions
----------------------

-- | The changes we can make to a store.
data StoreAction = forall a. (ValueString (Value a), ValueType (Value a))
                 => StoreSet Key (Value a)

-- | Apply the changes specified by the action to the store.
applyAction :: (Store s) => s -> StoreAction -> IO ()
applyAction store (StoreSet key val) = do
    _ <- set store key val
    return ()

----------------------
-- Merges
----------------------

-- | Attempt to merge to change histories together.  If the merge is successful, return a
-- list of 'StoreAction's.  For instance, this can fail if the histories have different
-- types.
tryMerge :: Key -> Maybe KeyHistory -> KeyHistory -> Maybe [StoreAction]
tryMerge key Nothing theirHistory =
    Just (insertNewActions key theirHistory)
tryMerge _ (Just (IntKeyHistory myTip myDiffs)) (IntKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge _ (Just (IntSetKeyHistory myTip myDiffs)) (IntSetKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge _ (Just (StringKeyHistory myTip myDiffs)) (StringKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge _ (Just (StringSetKeyHistory myTip myDiffs)) (StringSetKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge _ _ _ =
    Nothing

-- | Attempt to merge two histories of the same type together.  If the merge is
-- successful, return a list of 'StoreAction's.
merge :: Value a -> [Diff a] -> Value a -> [Diff a] -> Maybe [StoreAction]
merge _ _ _ _ = Nothing

-- | Prepare the actions that insert the entire key history into the store.
insertNewActions :: Key -> KeyHistory -> [StoreAction]
insertNewActions key (IntKeyHistory tip diffs) =
    map (StoreSet key) (reverse (diffsToValues tip diffs))
insertNewActions key (IntSetKeyHistory tip diffs) =
    map (StoreSet key) (reverse (diffsToValues tip diffs))
insertNewActions key (StringKeyHistory tip diffs) =
    map (StoreSet key) (reverse (diffsToValues tip diffs))
insertNewActions key (StringSetKeyHistory tip diffs) =
    map (StoreSet key) (reverse (diffsToValues tip diffs))

-- | Convert a tip and some diffs from it to values.
diffsToValues :: (Diffable a) => Value a -> [Diff a] -> [Value a]
diffsToValues tip diffs = tip : reverse (snd (foldl diffToValue (tip, []) diffs))
  where
    diffToValue (v, vs) diff = let v' = applyDiff v diff in (v', v' : vs)
