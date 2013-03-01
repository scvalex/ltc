module Ltc.Store.VersionControl (
       insertChangesInto
    ) where

import Ltc.Store.Class ( Store(..), Key )
import Ltc.Store.Serialization ( DiffPack(..), KeyHistory(..) )
import qualified Data.Map as M

-- | Insert the given changes into the store.  Returns a list of conflicting keys.
insertChangesInto :: (Store s) => s -> DiffPack -> IO [Key]
insertChangesInto _ (DiffPack m) = do
    return (M.keys m)
