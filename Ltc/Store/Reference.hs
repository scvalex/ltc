{-# LANGUAGE TypeFamilies #-}

module Ltc.Store.Reference (
        Reference, ConnectParameters(..)
    ) where

import Control.Monad ( when )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Digest.Pure.SHA ( sha1, showDigest )
import Ltc.Store.Class ( Store(..), Key, Value, Version )
import System.Directory ( createDirectory, doesDirectoryExist )
import System.FilePath ( (</>) )
import System.Log.Logger ( debugM )
import Text.Printf ( printf )

formatString :: String
formatString = "reference"

version :: Int
version = 1

tag :: String
tag = "Reference"

data Reference = Reference
    { getLocation :: FilePath
    }

instance Store Reference where
    data ConnectParameters Reference = ConnectParameters
        { location :: FilePath
        }

    open params = doOpen params

    close handle = doClose handle

    get _handle _key _version = undefined
    getLatest _handle _key = undefined

    set handle key value = doSet handle key value

    del _handle _key = undefined

doOpen :: ConnectParameters Reference -> IO Reference
doOpen params = do
    debugM tag "open"
    storeExists <- doesDirectoryExist (location params)
    when (not storeExists) (initStore (location params))
    return (Reference { getLocation = location params })

doClose :: Reference -> IO ()
doClose _handle = do
    debugM tag "close"
    return ()

doSet :: Reference -> Key -> Value -> IO Version
doSet reference key value = do
    debugM tag (printf "set %s" key)
    let keyHash = showDigest (sha1 (BL.pack key))
    BL.writeFile (getLocation reference </> "store" </> keyHash) value
    return 0

initStore :: FilePath -> IO ()
initStore loc = do
    debugM tag "initStore"
    createDirectory loc
    writeFile (loc </> "format") formatString
    writeFile (loc </> "version") (show version)
    createDirectory (loc </> "store")
