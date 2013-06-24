{-# LANGUAGE DeriveGeneric #-}

module Ltc.Changeset (
        -- * Changesets
        Changeset(..), changesetBaseVersion,
        Changes(..), changesFromList, changesToList, wireDiffForKey,

        -- * Serializable diffs
        WireDiff(..), wireDiffFromTo,
        wireDiffFromDiff, diffFromWireDiff
    ) where

import Data.ByteString.Char8 ( ByteString )
import Data.Function ( on )
import Data.Map ( Map )
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable(..), parse, printHum )
import Ltc.Store.Types ( Key, Storable, Version
                       , Type, typeOf )
import Ltc.Diff ( Diffable(..) )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Data.VectorClock as VC

----------------------
-- Changeset container
----------------------

data Changeset = Update { getBeforeUpdateVersion :: Version
                        , getAfterVersion        :: Version
                        , getChanges             :: Changes
                        }
               | Merge { getBeforeMergeVersions  :: (Version, Version)
                       , getMergeAncestorVersion :: Version
                       , getAfterVersion         :: Version
                       , getChanges              :: Changes
                       }
               deriving ( Show, Generic )

instance Serialize Changeset

instance Sexpable Changeset

-- Changesets are uniquely identified by their versions.
instance Eq Changeset where
    (==) = (==) `on` getAfterVersion

instance Ord Changeset where
    ch1 `compare` ch2 =
        let vsn1 = getAfterVersion ch1
            vsn2 = getAfterVersion ch2
        in if vsn1 == vsn2
           then EQ
           else if vsn1 `VC.causes` vsn2
                then LT
                else GT

-- | Get the 'Version' of the 'Changeset' that the given 'Changeset' is meant to be
-- applied over.
changesetBaseVersion :: Changeset -> Version
changesetBaseVersion (Update { getBeforeUpdateVersion = version }) = version
changesetBaseVersion (Merge { getMergeAncestorVersion = version }) = version

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

-- | Make a list of 'Key'-'WireDiff' pairs from a 'Changes'.
changesToList :: Changes -> [(Key, WireDiff)]
changesToList (Changes m) = M.toList m

-- | Get the 'WireDiff' associated with a 'Key' in a 'Changes'.
wireDiffForKey :: Changes -> Key -> Maybe WireDiff
wireDiffForKey (Changes m) key = M.lookup key m

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
    wireDiffFromDiff (diffFromTo before after)

-- | Get an untyped 'WireDiff' from a typed 'Diff'.
wireDiffFromDiff :: forall a. (Storable a) => Diff a -> WireDiff
wireDiffFromDiff diff =
    WireDiff { getWireDiffType = typeOf (undefined :: a)
             , getWireDiffDiff = BL.toStrict (printHum (toSexp diff))
             }

-- | Get a typed diff from a 'WireDiff'.  Fails if the parsing fails (i.e. if there is a
-- type mismatch).
diffFromWireDiff :: (Diffable a) => WireDiff -> Maybe (Diff a)
diffFromWireDiff wireDiff =
    case parse (BL.fromStrict (getWireDiffDiff wireDiff)) of
        Right [s] -> fromSexp s
        _         -> Nothing
