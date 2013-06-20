{-# LANGUAGE DeriveGeneric #-}

module Ltc.Changeset (
        -- * Changesets
        Changeset(..),
        Changes(..), changesFromList, wireDiffForKeyExn,

        -- * Serializable diffs
        WireDiff, wireDiffFromTo, getApplyWireDiff, diffFromWireDiff
    ) where

import Data.ByteString.Char8 ( ByteString )
import Data.Default ( def )
import Data.Map ( Map )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable(..), parse, printHum )
import Ltc.Store.Class ( Store(..), getLatestExn )
import Ltc.Store.Types ( Key, Storable, Version
                       , Type, typeOf )
import Ltc.Diff ( Diffable(..) )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M

----------------------
-- Changeset container
----------------------

data Changeset = Update { getBeforeUpdateVersion :: Version
                        , getAfterVersion        :: Version
                        , getChanges             :: Changes
                        }
               | Merge { getBeforeMergeVersions :: (Version, Version)
                       , getAfterVersion        :: Version
                       , getChanges             :: Changes
                       }
               deriving ( Show, Generic )

instance Serialize Changeset

instance Sexpable Changeset

-- Changesets are uniquely identified by their versions.
instance Eq Changeset where
    u1@(Update {}) == u2@(Update {}) =
        getBeforeUpdateVersion u1 == getBeforeUpdateVersion u2
        && getAfterVersion u1 == getAfterVersion u2
    m1@(Merge {}) == m2@(Merge {}) =
        getBeforeMergeVersions m1 == getBeforeMergeVersions m2
        && getAfterVersion m1 == getAfterVersion m2
    _ == _ =
        False

----------------------
-- Changes
----------------------

data Changes = Changes (Map Key WireDiff)
             deriving ( Show, Generic )

instance Serialize Changes

instance Sexpable Changes

-- | Make 'Changes' out of a list of 'Key'-'WireDiff' pairs.
changesFromList :: [(Key, WireDiff)] -> Changes
changesFromList = Changes . M.fromList

-- | Get the 'WireDiff' associated with a 'Key' in a 'Changes'.  Throw an error if the
-- 'Key' does not have an entry in the given 'Changes'.
wireDiffForKeyExn :: Changes -> Key -> WireDiff
wireDiffForKeyExn (Changes m) key =
    case M.lookup key m of
        Nothing       -> error "entry f or key not found in Changes"
        Just wireDiff -> wireDiff

----------------------
-- WireDiff
----------------------

data WireDiff = WireDiff
     { getWireDiffType :: Type
     , getWireDiffDiff :: ByteString
     } deriving ( Show, Generic )

instance Serialize WireDiff

instance Sexpable WireDiff

-- | Get a serializable diff between two 'Storable' values.
wireDiffFromTo :: (Storable a) => a -> a -> WireDiff
wireDiffFromTo before after =
    let diff = diffFromTo before after
    in WireDiff { getWireDiffType = typeOf before
                , getWireDiffDiff = BL.toStrict (printHum (toSexp diff))
                }

-- | Get a function that could apply a 'WireDiff' of the given type.
getApplyWireDiff :: forall a s. (Storable a, Store s)
                 => a           -- ^ dummy value to fix the type
                 -> (s -> Key -> WireDiff -> IO Bool)
getApplyWireDiff _ = \store key wireDiff -> do
    mtyp <- keyType store key
    case mtyp of
        Nothing -> do
            -- The key does not exist, so just assume 'def' for the previous value.
            let v = def :: a
            applyWireDiff store key v wireDiff
        Just typ -> do
            if typ == getWireDiffType wireDiff
                then do
                    (v :: a, _) <- getLatestExn store key
                    applyWireDiff store key v wireDiff
                else do
                    -- The key has the wrong type.
                    return False
  where
    -- | Apply the 'WireDiff' to the given value and set the given key to it.
    applyWireDiff :: (Store s) => s -> Key -> a -> WireDiff -> IO Bool
    applyWireDiff store key v wireDiff = do
        let Just diff = diffFromWireDiff wireDiff
        _ <- set store key (applyDiff v diff)
        return True

-- | Get a typed diff from a 'WireDiff'.  Fails if the parsing fails (i.e. if there is a
-- type mismatch).
diffFromWireDiff :: (Diffable a) => WireDiff -> Maybe (Diff a)
diffFromWireDiff wireDiff =
    case parse (BL.fromStrict (getWireDiffDiff wireDiff)) of
        Right [s] -> fromSexp s
        _         -> Nothing
