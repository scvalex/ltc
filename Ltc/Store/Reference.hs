{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TupleSections #-}

module Ltc.Store.Reference (
        Reference, ConnectParameters(..)
    ) where

import Control.Applicative
import qualified Control.Exception as CE
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Digest.Pure.SHA ( sha1, showDigest )
import Ltc.Store.Class ( Store(..), Key, Value, Version )
import System.Directory ( createDirectory, doesDirectoryExist )
import System.FilePath ( (</>) )
import System.Log.Logger ( debugM )
import Text.Printf ( printf )

formatString :: String
formatString = "reference"

storeVersion :: Int
storeVersion = 1

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

    get handle key version = doGet handle key version
    getLatest handle key = doGetLatest handle key

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

doGet :: Reference -> Key -> Version -> IO (Maybe Value)
doGet reference key _version = do
    debugM tag (printf "get %s" key)
    CE.handle (\(_ :: CE.IOException) -> return Nothing) $ do
        Just <$> BL.readFile (getLocation reference </> "store" </> keyHash key)

doGetLatest :: Reference -> Key -> IO (Maybe (Value, Version))
doGetLatest reference key = do
    value <- doGet reference key 0
    return ((,0) <$> value)

doSet :: Reference -> Key -> Value -> IO Version
doSet reference key value = do
    debugM tag (printf "set %s" key)
    BL.writeFile (getLocation reference </> "store" </> keyHash key) value
    return 1

initStore :: FilePath -> IO ()
initStore loc = do
    debugM tag "initStore"
    createDirectory loc
    writeFile (loc </> "format") formatString
    writeFile (loc </> "version") (show storeVersion)
    createDirectory (loc </> "store")

keyHash :: Key -> String
keyHash = showDigest . sha1 . BL.pack