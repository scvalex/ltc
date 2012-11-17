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
import System.Directory ( createDirectory, doesDirectoryExist, removeFile )
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

    del handle key = doDel handle key

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
doGet ref key _version = do
    debugM tag (printf "get %s" key)
    CE.handle (\(_ :: CE.IOException) -> return Nothing) $ do
        Just <$> BL.readFile (keyLocation ref key)

doGetLatest :: Reference -> Key -> IO (Maybe (Value, Version))
doGetLatest ref key = do
    value <- doGet ref key 0
    return ((,0) <$> value)

doSet :: Reference -> Key -> Value -> IO Version
doSet ref key value = do
    debugM tag (printf "set %s" key)
    BL.writeFile (keyLocation ref key) value
    return 1

doDel :: Reference -> Key -> IO (Maybe Version)
doDel ref key = do
    debugM tag (printf "del %s" key)
    CE.handle (\(_ :: CE.IOException) -> return Nothing) $ do
        removeFile (keyLocation ref key)
        return (Just 0)

initStore :: FilePath -> IO ()
initStore loc = do
    debugM tag "initStore"
    createDirectory loc
    writeFile (loc </> "format") formatString
    writeFile (loc </> "version") (show storeVersion)
    createDirectory (loc </> "store")

-- | The hash of a key.  This hash is used as the filename under which
-- the value is stored in the reference store.
keyHash :: Key -> String
keyHash = showDigest . sha1 . BL.pack

-- | The location of a key's value value.
keyLocation :: Reference -> Key -> FilePath
keyLocation ref key = getLocation ref </> "store" </> keyHash key
