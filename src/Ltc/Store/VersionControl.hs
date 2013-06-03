{-# LANGUAGE DeriveGeneric, ExistentialQuantification, FlexibleContexts #-}

-- | This module ties "Ltc.Diff" to "Ltc.Store".
module Ltc.Store.VersionControl (
        -- * Getting history
        DiffPack(..),
        KeyHistory(..), getDiffPack, getKeyHistory,

        -- * Selecting history
        versionsFromToIncluding,

        -- * Applying history
        insertChangesInto, Reason
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( forM )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Foldable ( foldlM )
import Data.Map ( Map )
import Data.Serialize ( Serialize )
import Data.Set ( Set )
import Data.VectorClock ( causes )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable(..) )
import Ltc.Store.Class ( Store(..), SetCmd(..), Storable
                       , typeOf
                       , Key, getExn, getLatestExn
                       , Version, keyVersionsExn )
import Ltc.Diff ( Diff, Diffable(..) )
import qualified Data.Map as M
import System.Log.Logger ( debugM )
import Text.Printf ( printf )

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "Simple"

----------------------
-- Wrappers around values and diffs
----------------------

-- FIXME We should include the typerep with the key history and get rid of the separate
-- cases.

-- | 'KeyHistory' achieves two goals.  First, it solidifies the type parameter of 'Diff'
-- by encoding the possibilities as a sum type.  Second, it encapsulates the current value
-- associated with a key, and the history of changes leading up to that point.
--
-- The diffs are reversed such that they can be applied to the tip.  So, the most recent
-- value is @tip@, the second most recent value is @applyDiff tip (head diffs)@, and so
-- on.
data KeyHistory = IntKeyHistory Integer [Diff Integer]
                | IntSetKeyHistory (Set Integer) [Diff (Set Integer)]
                | StringKeyHistory ByteString [Diff ByteString]
                | StringSetKeyHistory (Set ByteString) [Diff (Set ByteString)]
                deriving ( Eq, Generic, Show )

instance Sexpable KeyHistory

instance Serialize KeyHistory

-- | 'DiffPack' is just a map of 'Key's to 'KeyHistory's.
data DiffPack = DiffPack (Map Key KeyHistory)
              deriving ( Eq, Generic, Show )

instance Sexpable DiffPack

instance Serialize DiffPack

type Reason = String

----------------------
-- Applying history
----------------------

-- | Insert the given changes into the store.  Returns a list of conflicting keys.
insertChangesInto :: (Store s) => s -> DiffPack -> IO [(Key, Reason)]
insertChangesInto store (DiffPack m) = foldlM insertKeyHistory [] (M.toList m)
  where
    insertKeyHistory :: [(Key, Reason)] -> (Key, KeyHistory) -> IO [(Key, Reason)]
    insertKeyHistory conflicts (key, theirHistory) = do
        mmyHistory <- getKeyHistory store key
        case tryMerge key mmyHistory theirHistory of
            Right acts  -> do
                mapM_ (applyAction store) acts
                return conflicts
            Left reason -> do
                return ((key, reason) : conflicts)

----------------------
-- Store actions
----------------------

-- | Apply the changes specified by the action to the store.
applyAction :: (Store s) => s -> SetCmd -> IO ()
applyAction store (SetCmd key val) = do
    _ <- set store key val
    return ()

----------------------
-- Merges
----------------------

-- | Attempt to merge to change histories together.  If the merge is successful, return a
-- list of 'SetCmd's.  For instance, this can fail if the histories have different
-- types.
tryMerge :: Key -> Maybe KeyHistory -> KeyHistory -> Either Reason [SetCmd]
tryMerge key Nothing theirHistory =
    Right (insertNewActions key theirHistory)
tryMerge _ (Just (IntKeyHistory myTip myDiffs)) (IntKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge _ (Just (IntSetKeyHistory myTip myDiffs)) (IntSetKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge _ (Just (StringKeyHistory myTip myDiffs)) (StringKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge _ (Just (StringSetKeyHistory myTip myDiffs)) (StringSetKeyHistory theirTip theirDiffs) =
    merge myTip myDiffs theirTip theirDiffs
tryMerge _ _ _ =
    Left "different types"

-- | Attempt to merge two histories of the same type together.  If the merge is
-- successful, return a list of 'SetCmd's.
merge :: a -> [Diff a] -> a -> [Diff a] -> Either Reason [SetCmd]
merge _ _ _ _ = Left "key already exists"

-- | Prepare the actions that insert the entire key history into the store.
insertNewActions :: Key -> KeyHistory -> [SetCmd]
insertNewActions key (IntKeyHistory tip diffs) =
    map (SetCmd key) (reverse (diffsToValues tip diffs))
insertNewActions key (IntSetKeyHistory tip diffs) =
    map (SetCmd key) (reverse (diffsToValues tip diffs))
insertNewActions key (StringKeyHistory tip diffs) =
    map (SetCmd key) (reverse (diffsToValues tip diffs))
insertNewActions key (StringSetKeyHistory tip diffs) =
    map (SetCmd key) (reverse (diffsToValues tip diffs))

-- | Convert a tip and some diffs from it to values.
diffsToValues :: (Diffable a) => a -> [Diff a] -> [a]
diffsToValues tip diffs = tip : reverse (snd (foldl diffToValue (tip, []) diffs))
  where
    diffToValue (v, vs) diff = let v' = applyDiff v diff in (v', v' : vs)

----------------------
-- Selecting history
----------------------

-- | We often need to select changes /after/ one version clock, but before and including
-- another version clock.  The list of versions is in oldest-to-newest order.
versionsFromToIncluding :: (Store s) => s -> Key -> Version -> Version -> IO [Version]
versionsFromToIncluding store key from toInc = do
    debugM tag (printf "selecting VCs from %s to (including) %s"
                       (show from) (show toInc))
    vsns <- reverse <$> keyVersionsExn store key
    return $
        takeWhile (\vsn -> vsn `causes` toInc) $
        dropWhile (\vsn -> not (from `causes` vsn) || from == vsn) vsns

----------------------
-- Getting history
----------------------

-- | Get all the data from a store.
getDiffPack :: (Store s) => s -> IO DiffPack
getDiffPack store = do
    ks <- keys store
    DiffPack <$> foldlM addKeyHistory M.empty ks
  where
    addKeyHistory m key = do
        -- We just got the keys from the database, so the following cannot fail.
        Just kh <- getKeyHistory store key
        return (M.insert key kh m)

-- | Get the entire history of a key.  If the key is missing, return 'Nothing'.
getKeyHistory :: (Store s) => s -> Key -> IO (Maybe KeyHistory)
getKeyHistory store key = do
    mty <- keyType store key
    case mty of
        Nothing ->
            return Nothing
        Just ty | ty == typeOf (undefined :: Integer) -> do
            (tip :: Integer, _) <- getLatestExn store key
            diffs <- getDiffs tip
            return (Just (IntKeyHistory tip diffs))
        Just ty | ty == typeOf (undefined :: Set Integer) -> do
            (tip :: Set Integer, _) <- getLatestExn store key
            diffs <- getDiffs tip
            return (Just (IntSetKeyHistory tip diffs))
        Just ty | ty == typeOf (undefined :: ByteString) -> do
            (tip :: ByteString, _) <- getLatestExn store key
            diffs <- getDiffs tip
            return (Just (StringKeyHistory tip diffs))
        Just ty | ty == typeOf (undefined :: Set ByteString) -> do
            (tip :: Set ByteString, _) <- getLatestExn store key
            diffs <- getDiffs tip
            return (Just (StringSetKeyHistory tip diffs))
        Just _ ->
            error "getKeyHistory: wtf"
  where
    getDiffs :: (Storable a) => a -> IO [Diff a]
    getDiffs tip = do
        vsns <- keyVersionsExn store key
        -- @vsns@ contains at least the tip.
        vs <- forM (tail vsns) (\vsn -> getExn store key vsn)
        let (_, diffs) = foldl (\(v, ds) v' -> (v', diffFromTo v v' : ds))
                               (tip, [])
                               vs
        return (reverse diffs)
