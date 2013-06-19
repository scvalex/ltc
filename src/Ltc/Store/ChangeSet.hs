{-# LANGUAGE DeriveGeneric #-}

module Ltc.Store.ChangeSet (
        -- * Changesets
        ChangeSet(..), getChangeSetFromTo,

        -- * Serializable diffs
        WireDiff, getWireDiffFromTo, getApplyWireDiff
    ) where

import Data.ByteString.Char8 ( ByteString )
import Data.Map ( Map )
import Data.Serialize ( Serialize, encode, decode )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable )
import Ltc.Store.Class ( Store(..), getLatestExn )
import Ltc.Store.Types ( Key, Storable, Version
                       , Type, typeOf )
import Ltc.Diff ( Diffable(..) )
import System.Log.Logger ( debugM )
import Text.Printf ( printf )

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "ChangeSet"

----------------------
-- ChangeSet container
----------------------

data ChangeSet = Update { getBeforeUpdateVersion :: Version
                        , getAfterUpdateVersion  :: Version
                        , getUpdateChanges       :: Changes
                        }
               | Merge { getBeforeMergeVersions :: (Version, Version)
                       , getAfterMergeVersion   :: Version
                       , getMergeChanges        :: Changes
                       }
               deriving ( Show, Generic )

instance Serialize ChangeSet

instance Sexpable ChangeSet

-- ChangeSets are uniquely identified by their versions.
instance Eq ChangeSet where
    u1@(Update {}) == u2@(Update {}) =
        getBeforeUpdateVersion u1 == getBeforeUpdateVersion u2
        && getAfterUpdateVersion u1 == getAfterUpdateVersion u2
    m1@(Merge {}) == m2@(Merge {}) =
        getBeforeMergeVersions m1 == getBeforeMergeVersions m2
        && getAfterMergeVersion m1 == getAfterMergeVersion m2
    _ == _ =
        False

-- | Get the 'Update' 'ChangeSet' that moves a store from the formerstate to the latter
-- state.
getChangeSetFromTo :: (Store s) => s -> Version -> Version -> IO ChangeSet
getChangeSetFromTo store fromVsn toVsn = do
    changes <- getChangesFromTo store fromVsn toVsn
    return (Update { getBeforeUpdateVersion = fromVsn
                   , getAfterUpdateVersion  = toVsn
                   , getUpdateChanges       = changes
                   })

----------------------
-- Changes
----------------------

data Changes = Changes (Map Key WireDiff)
             deriving ( Show, Generic )

instance Serialize Changes

instance Sexpable Changes

-- | Get all the changes to a 'Store' that move it from the former state to the latter
-- state.
getChangesFromTo :: (Store s) => s -> Version -> Version -> IO Changes
getChangesFromTo _store fromVsn toVsn = do
    debugM tag (printf "getChangesFromTo %s %s" (show fromVsn) (show toVsn))
    let _ = getWireDiffFromTo (23 :: Integer) 42
    return undefined

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
getWireDiffFromTo :: (Storable a) => a -> a -> WireDiff
getWireDiffFromTo before after =
    let diff = diffFromTo before after
    in WireDiff { getWireDiffType = typeOf before
                , getWireDiffDiff = encode diff
                }

-- | Get a function that could apply a 'WireDiff' of the given type.
getApplyWireDiff :: forall a s. (Storable a, Store s)
                 => a           -- ^ dummy value to fix the type
                 -> (s -> Key -> WireDiff -> IO Bool)
getApplyWireDiff _ = \store key wireDiff -> do
    mtyp <- keyType store key
    case mtyp of
        Nothing -> do
            -- The key does not exist.
            -- FIXME If the key does not exist, we should just insert it.
            return False
        Just typ -> do
            if typ == getWireDiffType wireDiff
                then do
                    (v :: a, _) <- getLatestExn store key
                    let Right diff = decode (getWireDiffDiff wireDiff)
                    _ <- set store key (applyDiff v diff)
                    return True
                else do
                    -- The key has the wrong type.
                    return False
