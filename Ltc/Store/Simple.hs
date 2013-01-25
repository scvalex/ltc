{-# LANGUAGE TypeFamilies, TupleSections, DeriveDataTypeable #-}

-- | Imagine desiging a key-value store on top of the file system.
-- The 'Simple' store is basically that, with a few added
-- complications due to the versioning.
--
-- The store runs out of a single directory on disk.  The layout, as
-- created by 'open', is:
--
-- @
-- DB_DIR/
-- ├── format
-- ├── version
-- ├── nodeName
-- ├── tmp/
-- ├── keys/
-- └── values/
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
-- @DB_DIR/values@ is a directory that contains a file for each value
-- present in the key-value store.  The values in these files may be
-- gzipped.  These files are referenced by files in the @DB_DIR/keys@
-- directory.
module Ltc.Store.Simple (
        Simple, OpenParameters(..)
    ) where

import qualified Codec.Compression.GZip as Z
import Control.Applicative
import qualified Control.Exception as CE
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Data ( Data, Typeable )
import Data.Digest.Pure.SHA ( sha1, showDigest )
import qualified Data.VectorClock as VC
import Language.Sexp ( toSexp, fromSexp, parse, printHum )
import Ltc.Store.Class
import System.Directory ( createDirectory, doesFileExist, doesDirectoryExist
                        , renameFile )
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

data Simple = Simple { getBase           :: FilePath
                     , getUseCompression :: Bool
                     , getNodeName       :: NodeName
                     }

data KeyRecord = KR { getKeyName :: Key
                    , getVersions :: [(Version, ValueHash)]
                    } deriving ( Data, Typeable )

instance Store Simple where
    data OpenParameters Simple = OpenParameters
        { location       :: FilePath
        , useCompression :: Bool
        , nodeName       :: ByteString
        }

    open params = doOpen params

    close handle = doClose handle

    get handle key version = doGet handle key version
    getLatest handle key = doGetLatest handle key

    set handle key value = doSet handle key value

doOpen :: OpenParameters Simple -> IO Simple
doOpen params = do
    debugM tag "open"
    storeExists <- doesDirectoryExist (location params)
    when (not storeExists) (initStore params)
    nn <- BL.readFile (locationNodeName (location params))
    when (nn /= nodeName params) $
        fail (printf "node name mismatch: store has %s" (BL.unpack nn))
    return (Simple { getBase           = location params
                   , getUseCompression = useCompression params
                   , getNodeName       = nodeName params
                   })

doClose :: Simple -> IO ()
doClose _handle = do
    debugM tag "close"
    return ()

doGet :: Simple -> Key -> Version -> IO (Maybe Value)
doGet ref key version = do
    -- FIXME Add a PrintfArg instance for lazy ByteStrings
    debugM tag (printf "get %s" (BL.unpack key))
    CE.handle (\(_ :: CE.IOException) -> return Nothing) $ do
        withKeyRecord (locationKey ref key) $ \kr -> do
            case lookup version (getVersions kr) of
                Nothing -> return Nothing
                Just vhash -> do
                    Just . (if getUseCompression ref then Z.decompress else id)
                        <$> BL.readFile (locationValueHash ref vhash)

doGetLatest :: Simple -> Key -> IO (Maybe (Value, Version))
doGetLatest ref key = do
    withKeyRecord (locationKey ref key) $ \kr -> do
        let latestVersion = fst (head (getVersions kr))
        Just value <- doGet ref key latestVersion
        return (Just (value, latestVersion))

doSet :: Simple -> Key -> Value -> IO Version
doSet ref key value = do
    debugM tag (printf "set %s" (BL.unpack key))
    let vhash = valueHash value
    atomicWriteFile ref (locationValueHash ref vhash)
        ((if getUseCompression ref then Z.compress else id) value)
    setKeyRecord ref (locationKey ref key) $ \mkrOld -> do
        let krOld = maybe (KR { getKeyName = key, getVersions = [] }) id mkrOld
            nn = getNodeName ref
            versions = getVersions krOld
            versions' = case versions of
                []   -> [(VC.insert nn 1 VC.empty, vhash)]
                v@(vc, _):vs -> (VC.incWithDefault nn vc 0, vhash) : v : vs
        return (krOld { getVersions = versions' })

----------------------
-- Helpers
----------------------

setKeyRecord :: Simple -> FilePath -> (Maybe KeyRecord -> IO KeyRecord) -> IO Version
setKeyRecord ref path update = do
    mkr <- readKeyRecord path
    kr' <- update mkr
    atomicWriteFile ref path (printHum (toSexp kr'))
    return (fst (head (getVersions kr')))

-- | Wrapper around 'readKeyRecord'.
withKeyRecord :: FilePath -> (KeyRecord -> IO (Maybe a)) -> IO (Maybe a)
withKeyRecord path f = do
    mkr <- readKeyRecord path
    case mkr of
        Nothing -> return Nothing
        Just kr -> f kr

-- | Read a key record from disk.  If the key doesn't exist, return
-- 'Nothing'.  If the key record is corrupt, fail.
readKeyRecord :: FilePath -> IO (Maybe KeyRecord)
readKeyRecord path = do
    keyExists <- doesFileExist path
    if keyExists
          then do
              text <- BL.readFile path
              case parse text of
                  Left err -> fail (printf "corrupt key file: %s (%s)" path (show err))
                  Right [s] ->
                      case fromSexp s of
                          Nothing -> fail (printf "corrupt key file: %s (KeyRecord)" path)
                          Just kr -> return (Just kr)
                  Right _ -> fail (printf "corrupt key file: %s (multiple sexps)" path)
          else return Nothing

-- | Write the given 'ByteString' to the file atomically.  Overwrite
-- any previous content.  The 'Simple' reference is needed in order to
-- find the temporary directory.
atomicWriteFile :: Simple -> FilePath -> ByteString -> IO ()
atomicWriteFile ref path content = do
    (tempFile, handle) <- openBinaryTempFile (locationTemporary (getBase ref)) "ltc"
    BL.hPut handle content `CE.finally` hClose handle
    renameFile tempFile path

-- | Create the initial layout for the store at the given directory
-- base.
initStore :: OpenParameters Simple -> IO ()
initStore params = do
    debugM tag "initStore"
    let base = location params
    createDirectory base
    writeFile (locationFormat base) formatString
    writeFile (locationVersion base) (show storeVersion)
    BL.writeFile (locationNodeName base) (nodeName params)
    createDirectory (locationTemporary base)
    createDirectory (locationValues base)
    createDirectory (locationKeys base)

-- | The hash of a key.  This hash is used as the filename under which
-- the key is stored in the @keys/@ folder.
keyHash :: Key -> KeyHash
keyHash = BL.pack . showDigest . sha1

-- | The hash of a value.  This hash is used as the filename under
-- which the value is stored in the @values/@ folder.
valueHash :: Value -> ValueHash
valueHash = BL.pack . showDigest . sha1

----------------------
-- Locations
----------------------

-- | The location of a key's value.
locationValueHash :: Simple -> ValueHash -> FilePath
locationValueHash ref hash = locationValues (getBase ref) </> BL.unpack hash

-- | The location of a key's record.
locationKey :: Simple -> Key -> FilePath
locationKey ref key = locationKeys (getBase ref) </> BL.unpack (keyHash key)

locationFormat :: FilePath -> FilePath
locationFormat base = base </> "format"

locationVersion :: FilePath -> FilePath
locationVersion base = base </> "version"

locationTemporary :: FilePath -> FilePath
locationTemporary base = base </> "tmp"

locationValues :: FilePath -> FilePath
locationValues base = base </> "values"

locationKeys :: FilePath -> FilePath
locationKeys base = base </> "keys"

locationNodeName :: FilePath -> FilePath
locationNodeName base = base </> "nodeName"
