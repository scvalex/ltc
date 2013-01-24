{-# LANGUAGE TypeFamilies, TupleSections #-}

-- | Imagine desiging a key-value store on top of the file system.
-- The 'Simple' store is basically that, with a few added
-- complications due to the versioning.
--
-- The store runs out of a single directory on disk.  The layout is:
--
-- @
-- DB_DIR/
-- ├── format
-- ├── version
-- ├── tmp/
-- ├── keys/
-- └── objects/
-- @
--
-- @DB_DIR/format@ contains a single string identifying the format of
-- the key-value store.  In our case, this is "simple".
--
-- @DB_DIR/version@ contains the version of the key-value store.  This
-- is used to upgrade on-disk files.
--
-- @DB_DIR/tmp@ is a directory used as a staging ground for creating
-- new files.  On most file systems, /move/ is an atomic operation,
-- but /create file and write to it/ is not.  So, when writing to
-- disk, we usually create a new file in this temporary directory,
-- then move it to its real location.
--
-- @DB_DIR/keys@ is a directory that contains a file for each key
-- present in the key-value store.  Each of these files is an
-- S-Expression which contains meta information about the value of the
-- key.
--
-- @DB_DIR/objects@ is a directory that contains a file for each value
-- present in the key-value store.  The values in these files may be
-- gzipped.  These files are referenced by files in the @DB_DIR/keys@
-- directory.
module Ltc.Store.Simple (
        Simple, ConnectParameters(..)
    ) where

import qualified Codec.Compression.GZip as Z
import Control.Applicative
import qualified Control.Exception as CE
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Digest.Pure.SHA ( sha1, showDigest )
import Ltc.Store.Class ( Store(..), Key, Value, Version )
import System.Directory ( createDirectory, doesDirectoryExist, removeFile, renameFile )
import System.FilePath ( (</>) )
import System.IO ( hClose, openBinaryTempFile )
import System.Log.Logger ( debugM )
import Text.Printf ( printf )

formatString :: String
formatString = "simple"

storeVersion :: Int
storeVersion = 2

tag :: String
tag = "Simple"

data Simple = Simple
    { getLocation :: FilePath
    }

instance Store Simple where
    data ConnectParameters Simple = ConnectParameters
        { location :: FilePath
        }

    open params = doOpen params

    close handle = doClose handle

    get handle key version = doGet handle key version
    getLatest handle key = doGetLatest handle key

    set handle key value = doSet handle key value

    del handle key = doDel handle key

doOpen :: ConnectParameters Simple -> IO Simple
doOpen params = do
    debugM tag "open"
    storeExists <- doesDirectoryExist (location params)
    when (not storeExists) (initStore (location params))
    return (Simple { getLocation = location params })

doClose :: Simple -> IO ()
doClose _handle = do
    debugM tag "close"
    return ()

doGet :: Simple -> Key -> Version -> IO (Maybe Value)
doGet ref key _version = do
    debugM tag (printf "get %s" key)
    CE.handle (\(_ :: CE.IOException) -> return Nothing) $ do
        Just . Z.decompress <$> BL.readFile (keyLocation ref key)

doGetLatest :: Simple -> Key -> IO (Maybe (Value, Version))
doGetLatest ref key = do
    value <- doGet ref key 0
    return ((,1) <$> value)

doSet :: Simple -> Key -> Value -> IO Version
doSet ref key value = do
    debugM tag (printf "set %s" key)
    (tempFile, handle) <- openBinaryTempFile (getLocation ref </> "tmp") "ltc"
    BL.hPut handle (Z.compress value)
    hClose handle
    renameFile tempFile (keyLocation ref key)
    return 1

doDel :: Simple -> Key -> IO (Maybe Version)
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
    createDirectory (loc </> "tmp")

-- | The hash of a key.  This hash is used as the filename under which
-- the value is stored in the reference store.
keyHash :: Key -> String
keyHash = showDigest . sha1 . BL.pack

-- | The location of a key's value value.
keyLocation :: Simple -> Key -> FilePath
keyLocation ref key = getLocation ref </> "store" </> keyHash key
