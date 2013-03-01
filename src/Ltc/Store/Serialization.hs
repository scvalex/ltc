{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}

module Ltc.Store.Serialization (
        DiffPack(..),

        KeyHistory(..), getDiffPack, getKeyHistory
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( forM )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Foldable ( foldlM )
import Data.Map ( Map )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable(..) )
import Ltc.Store.Class ( Store(..), Key
                       , Value(..), ValueString(..), Type(..), Single, Collection )
import Ltc.Store.Diff ( Diff, Diffable(..) )
import qualified Data.Map as M
import Text.Printf ( printf )

----------------------
-- Wrappers around values and diffs
----------------------

-- | 'KeyHistory' achieves two goals.  First, it solidifies the type parameter of 'Value'
-- by encoding the possibilities as a sum type.  Second, it encapsulates the current value
-- associated with a key, and the history of changes leading up to that point.
--
-- The diffs are reversed such that they can be applied to the tip.  So, the most recent
-- value is @tip@, the second most recent value is @applyDiff tip (head diffs)@, and so
-- on.
data KeyHistory = IntKeyHistory (Value (Single Integer)) [Diff (Single Integer)]
                | IntSetKeyHistory (Value (Collection Integer)) [Diff (Collection Integer)]
                | StringKeyHistory (Value (Single ByteString)) [Diff (Single ByteString)]
                | StringSetKeyHistory (Value (Collection ByteString))
                                      [Diff (Collection ByteString)]
                deriving ( Generic )

instance Sexpable KeyHistory

-- | 'DiffPack' is just a map of 'Key's to 'KeyHistory's.
data DiffPack = DiffPack (Map Key KeyHistory)
              deriving ( Generic )

instance Sexpable DiffPack

----------------------
-- Helpers
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
        Just SingleInteger -> do
            (tip :: Value (Single Integer), _) <- storeUnJust =<< getLatest store key
            diffs <- getDiffs tip
            return (Just (IntKeyHistory tip diffs))
        Just CollectionInteger -> do
            (tip :: Value (Collection Integer), _) <- storeUnJust =<< getLatest store key
            diffs <- getDiffs tip
            return (Just (IntSetKeyHistory tip diffs))
        Just SingleString -> do
            (tip :: Value (Single ByteString), _) <- storeUnJust =<< getLatest store key
            diffs <- getDiffs tip
            return (Just (StringKeyHistory tip diffs))
        Just CollectionString -> do
            (tip :: Value (Collection ByteString), _) <- storeUnJust =<< getLatest store key
            diffs <- getDiffs tip
            return (Just (StringSetKeyHistory tip diffs))
  where
    getDiffs :: (ValueString (Value a), Diffable a)
             => Value a -> IO [Diff a]
    getDiffs tip = do
        vsns <- storeUnJust =<< keyVersions store key
        -- @vsns@ contains at least the tip.
        vs <- forM (tail vsns) (\vsn -> storeUnJust =<< get store key vsn)
        let (_, diffs) = foldl (\(v, ds) v' -> (v', reverseDiff (diffFromTo v v') : ds))
                               (tip, [])
                               vs
        return diffs

    storeUnJust (Just a) = return a
    storeUnJust Nothing  = fail (printf "could not find value for %s in store" (show key))
