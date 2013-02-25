{-# LANGUAGE TypeFamilies, TupleSections, DeriveGeneric, FlexibleContexts #-}

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
import GHC.Generics ( Generic )
import Data.Digest.Pure.SHA ( sha1, showDigest )
import Data.Foldable ( find, foldlM )
import Data.Set ( Set )
import qualified Data.Set as S
import qualified Data.VectorClock as VC
import Language.Sexp ( Sexpable(..), parse, printHum )
import Ltc.Store.Class
import System.Directory ( createDirectory, doesFileExist, doesDirectoryExist
                        , renameFile, getDirectoryContents )
import System.FilePath ( (</>) )
import System.IO ( hClose, openBinaryTempFile )
import System.Log.Logger ( debugM )
import Text.Printf ( printf )

formatString :: String
formatString = "simple"

storeVsn :: Int
storeVsn = 4

tag :: String
tag = "Simple"

data Simple = Simple { getBase           :: FilePath
                     , getUseCompression :: Bool
                     , getNodeName       :: NodeName
                     }

-- | There is one 'KeyVersion' record for each *value* stored for a
-- key.  So, a key whose value was set, and then changed twice, will
-- have three of these records.
data KeyVersion = KeyVersion { getVersion   :: Version
                             , getValueHash :: ValueHash
                             } deriving ( Generic )

instance Sexpable KeyVersion

-- | Represents a single 'Key' with at least one value ('getTip'), and
-- possibly many older values ('getHistory').  History is ordered
-- most-recent-first.  The values of a key are of a particular type
-- ('getValueType'); this is determined when the key is first created,
-- and cannot be later changed.
data KeyRecord = KR { getKeyName   :: Key
                    , getValueType :: Type
                    , getTip       :: KeyVersion
                    , getHistory   :: [KeyVersion]
                    } deriving ( Generic )

instance Sexpable KeyRecord

instance Store Simple where
    data OpenParameters Simple = OpenParameters
        { location       :: FilePath
        , useCompression :: Bool
        , nodeName       :: ByteString
        }

    open params = doOpen params

    close ref = doClose ref

    storeFormat _ = formatString
    storeVersion _ = storeVsn

    get ref key version = doGet ref key version
    getLatest ref key = doGetLatest ref key

    keyVersions ref key = doKeyVersions ref key

    set ref key value = doSet ref key value

    keys ref = doKeys ref

doOpen :: OpenParameters Simple -> IO Simple
doOpen params = do
    debugM tag "open"
    storeExists <- doesDirectoryExist (location params)
    when (not storeExists) (initStore params)
    nn <- BL.readFile (locationNodeName (location params))
    when (nn /= nodeName params) $
        CE.throw (NodeNameMismatchError { requestedName = nodeName params
                                        , storeName     = nn })
    return (Simple { getBase           = location params
                   , getUseCompression = useCompression params
                   , getNodeName       = nodeName params
                   })

doClose :: Simple -> IO ()
doClose _handle = do
    -- FIXME We should wait for other commands to finish before closing.
    debugM tag "close"
    return ()

doGet :: (ValueString (Value a))
      => Simple -> Key -> Version -> IO (Maybe (Value a))
doGet ref key version = do
    debugM tag (printf "get %s" (show key))
    CE.handle (\(exn :: CE.IOException) -> do
                CE.throw (CorruptKeyFileError { keyFilePath = locationKey ref key
                                              , ckfReason   = show exn })) $ do
        withKeyRecord (locationKey ref key) $ \kr -> do
            case findVersion version kr of
                Nothing -> do
                    return Nothing
                Just kvsn -> do
                    let valueFile = locationValueHash ref (getValueHash kvsn)
                    s <- (if getUseCompression ref then Z.decompress else id)
                         <$> BL.readFile valueFile
                    -- FIXME It's not enough to parse the value; we should also check its
                    -- recorded type.
                    case unValueString s of
                        Nothing -> CE.throw (CorruptValueFileError {
                                                  valueFilePath = valueFile,
                                                  cvfReason     = "unparsable" })
                        Just v -> return (Just v)

doGetLatest :: (ValueString (Value a))
            => Simple -> Key -> IO (Maybe (Value a, Version))
doGetLatest ref key = do
    withKeyRecord (locationKey ref key) $ \kr -> do
        let latestVersion = getVersion (getTip kr)
        -- Every key has at least one value.
        mvalue <- doGet ref key latestVersion
        case mvalue of
            Just value -> return (Just (value, latestVersion))
            Nothing -> CE.throw (CorruptStoreError (printf "%s's tip (%s) does not exist"
                                                           (show (locationKey ref key))
                                                           (show latestVersion)))

doKeyVersions :: Simple -> Key -> IO (Maybe [Version])
doKeyVersions ref key = do
    withKeyRecord (locationKey ref key) $ \kr -> do
        return . Just . map getVersion $ getTip kr : getHistory kr

doSet :: (ValueString (Value a), ValueType (Value a))
      => Simple -> Key -> Value a -> IO Version
doSet ref key value = do
    debugM tag (printf "set %s" (show key))
    let vhash = valueHash value
    atomicWriteFile ref (locationValueHash ref vhash)
        ((if getUseCompression ref then Z.compress else id) (valueString value))
    setKeyRecord ref (locationKey ref key) $ \mkrOld -> do
        let nn = getNodeName ref
        case mkrOld of
            Nothing -> return $
                KR { getKeyName = key
                   , getValueType = valueType value
                   , getTip     = KeyVersion { getVersion   = VC.insert nn 1 VC.empty
                                             , getValueHash = vhash }
                   , getHistory = []
                   }
            Just krOld -> do
                when (getValueType krOld /= valueType value) $ do
                    CE.throw (TypeMismatchError { expectedType = getValueType krOld
                                                , foundType    = valueType value })
                let v = getTip krOld
                return $ krOld { getTip = KeyVersion { getVersion   = VC.incWithDefault
                                                                          nn (getVersion v) 0
                                                     , getValueHash = vhash
                                                     }
                               , getHistory = v : getHistory krOld
                               }

doKeys :: Simple -> IO (Set Key)
doKeys ref = do
    debugM tag "keys"
    let keysDir = locationKeys (getBase ref)
    kfs <- getDirectoryContents keysDir
    foldlM
        (\s kf -> do
          mk <- withKeyRecord (keysDir </> kf) (\kr -> return (Just (getKeyName kr)))
          return (maybe s (\k -> S.insert k s) mk))
        S.empty
        kfs

----------------------
-- Helpers
----------------------

setKeyRecord :: Simple -> FilePath -> (Maybe KeyRecord -> IO KeyRecord) -> IO Version
setKeyRecord ref path update = do
    mkr <- readKeyRecord path
    kr' <- update mkr
    atomicWriteFile ref path (printHum (toSexp kr'))
    return (getVersion (getTip kr'))

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
              let mkErr reason = CorruptKeyFileError { keyFilePath = path
                                                     , ckfReason  = reason }
              case parse text of
                  Left err ->
                      CE.throw (mkErr (show err))
                  Right [s] ->
                      case fromSexp s of
                          Nothing -> CE.throw (mkErr "invalid sexp")
                          Just kr -> return (Just kr)
                  Right _ ->
                      CE.throw (mkErr "multiple sexps")
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
    writeFile (locationVersion base) (show storeVsn)
    BL.writeFile (locationNodeName base) (nodeName params)
    createDirectory (locationTemporary base)
    createDirectory (locationValues base)
    createDirectory (locationKeys base)

-- | The hash of a key.  This hash is used as the filename under which
-- the key is stored in the @keys/@ folder.
keyHash :: Key -> KeyHash
keyHash (Key k) = BL.pack (showDigest (sha1 k))

-- | The hash of a value.  This hash is used as the filename under
-- which the value is stored in the @values/@ folder.
valueHash :: (ValueString (Value a)) => Value a -> ValueHash
valueHash = BL.pack . showDigest . sha1 . valueString

-- | Find the 'KeyVersion' with the given 'Version'.
findVersion :: Version -> KeyRecord -> Maybe KeyVersion
findVersion vsn kr = find (\kv -> getVersion kv == vsn) (getTip kr : getHistory kr)

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
