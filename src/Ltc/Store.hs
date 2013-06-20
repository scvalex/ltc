module Ltc.Store (
        module Ltc.Diff,
        module Ltc.Store.Class,
        module Ltc.Store.Event
    ) where

-- FIXME Rename Simple to FileSystem
-- FIXME Move everything that's note a Store above Store/
import Ltc.Diff
import Ltc.Store.Class
import Ltc.Store.Event
