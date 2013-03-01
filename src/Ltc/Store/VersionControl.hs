{-# LANGUAGE ExistentialQuantification #-}

module Ltc.Store.VersionControl (
       insertChangesInto
    ) where

import Data.Foldable ( foldlM )
import Ltc.Store.Class ( Store(..), Value, Key )
import Ltc.Store.Diff ( Diff )
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
        case tryMerge mmyHistory theirHistory of
            Just acts  -> do
                mapM_ (applyAction store) acts
                return conflicts
            Nothing -> do
                return (key:conflicts)

----------------------
-- Store actions
----------------------

-- | The changes we can make to a store.
data StoreAction = forall a. StoreSet Key (Value a)

-- | Apply the changes specified by the action to the store.
applyAction :: (Store s) => s -> StoreAction -> IO ()
applyAction _ _ = return ()

----------------------
-- Merges
----------------------

-- | Attempt to merge to change histories together.  If the merge is successful, return a
-- list of 'StoreAction's.  For instance, this can fail if the histories have different
-- types.
tryMerge :: Maybe KeyHistory -> KeyHistory -> Maybe [StoreAction]
tryMerge Nothing theirHistory =
    Just (insertNewActions theirHistory)
tryMerge (Just (IntKeyHistory myTip myDiffs)) (IntKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge (Just (IntSetKeyHistory myTip myDiffs)) (IntSetKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge (Just (StringKeyHistory myTip myDiffs)) (StringKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge (Just (StringSetKeyHistory myTip myDiffs)) (StringSetKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge _ _ =
    Nothing

-- | Attempt to merge two histories of the same type together.  If the merge is
-- successful, return a list of 'StoreAction's.
merge :: Value a -> [Diff a] -> Value a -> [Diff a] -> Maybe [StoreAction]
merge _ _ _ _ = Just []

-- | Prepare the actions that insert the entire key history into the store.
insertNewActions :: KeyHistory -> [StoreAction]
insertNewActions _ = []
