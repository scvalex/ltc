{-# LANGUAGE TypeFamilies #-}

module Ltc.Store.Reference (
        Reference(..)
    ) where

import Ltc.Store.Class ( Store(..) )

data Reference = Reference
    { location :: FilePath
    }

instance Store Reference where
    data ConnectParameters = ConnectParameters

    connect _params = undefined
    close _handle = undefined

    get _handle _key _version = undefined
    getLatest _handle _key = undefined

    set _handle _key _value = undefined

    del _handle _key = undefined
