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
import Control.Applicative ( (<$>) )
import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar_, readMVar )
import Control.Concurrent.STM ( atomically, writeTChan )
import qualified Control.Exception as CE
import Control.Monad ( when, unless )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.Char8 ( ByteString )
import GHC.Generics ( Generic )
import Data.Digest.Pure.SHA ( sha1, showDigest, integerDigest )
import Data.Foldable ( find, foldlM )
import Data.Set ( Set )
import qualified Data.Set as S
import qualified Data.VectorClock as VC
import Language.Sexp ( Sexpable(..), parse, printHum )
import Ltc.Store.Class ( Store(..), ValueType(..), ValueString(..), Type(..)
                       , Value, ValueHash, Version
                       , Key(..), KeyHash
                       , NodeName
                       , TypeMismatchError(..), CorruptStoreError(..), CorruptKeyFileError(..)
                       , StoreClosed(..), CorruptValueFileError(..), NodeNameMismatchError(..) )
import Ltc.Store.Event ( EventChannel, Event(..) )
import System.Directory ( createDirectory, doesFileExist, doesDirectoryExist
                        , renameFile, getDirectoryContents )
import System.FilePath ( (</>) )
import System.IO ( hClose, openBinaryTempFile )
import System.Log.Logger ( debugM )
import Text.Printf ( printf )

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "Simple"

----------------------
-- Simple store identifiers
----------------------

formatString :: String
formatString = "simple"

storeVsn :: Int
storeVsn = 4

----------------------
-- Types
----------------------

data Simple = Simple
    { getBase           :: FilePath
    , getUseCompression :: Bool
    , getNodeName       :: NodeName
    , getEventChannels  :: MVar [EventChannel]
    , getIsOpen         :: MVar Bool
    }

-- | There is one 'KeyVersion' record for each *value* stored for a
-- key.  So, a key whose value was set, and then changed twice, will
-- have three of these records.
data KeyVersion = KeyVersion
    { getVersion   :: Version
    , getValueHash :: ValueHash
    } deriving ( Generic )

instance Sexpable KeyVersion

-- | Represents a single 'Key' with at least one value ('getTip'), and
-- possibly many older values ('getHistory').  History is ordered
-- most-recent-first.  The values of a key are of a particular type
-- ('getValueType'); this is determined when the key is first created,
-- and cannot be later changed.
data KeyRecord = KR
    { getKeyName   :: Key
    , getValueType :: Type
    , getTip       :: KeyVersion
    , getHistory   :: [KeyVersion]
    } deriving ( Generic )

instance Sexpable KeyRecord

----------------------
-- Store interface
----------------------

instance Store Simple where
    -- | Store configuration.
    data OpenParameters Simple = OpenParameters
        { -- | The store directory.
          location        :: FilePath
          -- | Whether files should be gzip'd.
        , useCompression  :: Bool
          -- | The name of the node opening the store.  If the name stored on disk is
          -- different from this, a 'NodeNameMismatchError' will be thrown (unless
          -- 'forceOpen' is set).
        , nodeName        :: ByteString
          -- | Create the store if it is missing.  If this is not set and the store is
          -- missing, throw 'CorruptStoreError'.
        , createIfMissing :: Bool
          -- | Open the store even if there is a node name mismatch.
        , forceOpen       :: Bool
        }

    open params = doOpen params

    close store = doClose store

    storeFormat _ = formatString
    storeVersion _ = storeVsn

    get store key version = doGet store key version
    getLatest store key = doGetLatest store key

    keyVersions store key = doKeyVersions store key

    keyType store key = doKeyType store key

    set store key value = doSet store key value

    keys store = doKeys store

    addEventChannel store eventChannel = doAddEventChannel store eventChannel

doOpen :: OpenParameters Simple -> IO Simple
doOpen params = do
    debugM tag (printf "open store '%s'" (location params))
    storeExists <- doesDirectoryExist (location params)
    when (not storeExists) $ do
        if createIfMissing params
            then initStore params
            else CE.throw (CorruptStoreError { csReason = "missing" })
    nn <- BL.readFile (locationNodeName (location params))
    when (not (forceOpen params) && nn /= nodeName params) $
        CE.throw (NodeNameMismatchError { requestedName = nodeName params
                                        , storeName     = nn })
    eventChannels <- newMVar []
    isOpen <- newMVar True
    return (Simple { getBase           = location params
                   , getUseCompression = useCompression params
                   , getNodeName       = nodeName params
                   , getEventChannels  = eventChannels
                   , getIsOpen         = isOpen
                   })

doClose :: Simple -> IO ()
doClose store = do
    debugM tag (printf "close store '%s'" (getBase store))
    modifyMVar_ (getIsOpen store) (const (return False))
    writeEventChannels store CloseEvent

doGet :: (ValueString (Value a))
      => Simple -> Key -> Version -> IO (Maybe (Value a))
doGet store key version = do
    debugM tag (printf "get %s" (show key))
    writeEventChannels store getEvent
    CE.handle (\(exn :: CE.IOException) -> do
                CE.throw (CorruptKeyFileError { keyFilePath = locationKey store key
                                              , ckfReason   = show exn })) $ do
        withKeyRecord (locationKey store key) $ \kr -> do
            case findVersion version kr of
                Nothing -> do
                    return Nothing
                Just kvsn -> do
                    let valueFile = locationValueHash store (getValueHash kvsn)
                    s <- (if getUseCompression store then Z.decompress else id)
                         <$> BL.readFile valueFile
                    -- FIXME It's not enough to parse the value; we should also check its
                    -- recorded type.
                    case unValueString s of
                        Nothing -> CE.throw (CorruptValueFileError {
                                                  valueFilePath = valueFile,
                                                  cvfReason     = "unparsable" })
                        Just v -> return (Just v)
  where
    getEvent =
        let Key k = key
        in GetEvent { eventKey  = key
                    , keyDigest = fromInteger (integerDigest (sha1 k))
                    }

doGetLatest :: (ValueString (Value a))
            => Simple -> Key -> IO (Maybe (Value a, Version))
doGetLatest store key = do
    debugM tag (printf "getLatest %s" (show key))
    withKeyRecord (locationKey store key) $ \kr -> do
        let latestVersion = getVersion (getTip kr)
        -- Every key has at least one value.
        mvalue <- doGet store key latestVersion
        case mvalue of
            Just value -> return (Just (value, latestVersion))
            Nothing -> CE.throw (CorruptStoreError (printf "%s's tip (%s) does not exist"
                                                           (show (locationKey store key))
                                                           (show latestVersion)))

doKeyVersions :: Simple -> Key -> IO (Maybe [Version])
doKeyVersions store key = do
    debugM tag (printf "keyVersions %s" (show key))
    withKeyRecord (locationKey store key) $ \kr -> do
        return . Just . map getVersion $ getTip kr : getHistory kr

doKeyType :: Simple -> Key -> IO (Maybe Type)
doKeyType store key = do
    debugM tag (printf "keyType %s" (show key))
    withKeyRecord (locationKey store key) $ \kr -> do
        return (Just (getValueType kr))

doSet :: (ValueString (Value a), ValueType (Value a))
      => Simple -> Key -> Value a -> IO Version
doSet store key value = do
    assertIsOpen store
    debugM tag (printf "set %s" (show key))
    writeEventChannels store setEvent
    let vhash = valueHash value
    atomicWriteFile store (locationValueHash store vhash)
        ((if getUseCompression store then Z.compress else id) (valueString value))
    setKeyRecord store (locationKey store key) $ \mkrOld -> do
        let nn = getNodeName store
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
  where
    setEvent =
        let Key k = key
            v = valueString value
        in SetEvent { eventKey    = key
                    , keyDigest   = fromInteger (integerDigest (sha1 k))
                    , valueDigest = fromInteger (integerDigest (sha1 v))
                    }

doKeys :: Simple -> IO (Set Key)
doKeys store = do
    debugM tag "keys"
    let keysDir = locationKeys (getBase store)
    kfs <- getDirectoryContents keysDir
    foldlM
        (\s kf -> do
          mk <- withKeyRecord (keysDir </> kf) (\kr -> return (Just (getKeyName kr)))
          return (maybe s (\k -> S.insert k s) mk))
        S.empty
        kfs

doAddEventChannel :: Simple -> EventChannel -> IO ()
doAddEventChannel store eventChannel = do
    modifyMVar_ (getEventChannels store) $ \eventChannels ->
        return (eventChannel : eventChannels)

----------------------
-- Helpers
----------------------

setKeyRecord :: Simple -> FilePath -> (Maybe KeyRecord -> IO KeyRecord) -> IO Version
setKeyRecord store path update = do
    mkr <- readKeyRecord path
    kr' <- update mkr
    atomicWriteFile store path (printHum (toSexp kr'))
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
atomicWriteFile store path content = do
    (tempFile, handle) <- openBinaryTempFile (locationTemporary (getBase store)) "ltc"
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

-- | Write event to all event channels
writeEventChannels :: Simple -> Event -> IO ()
writeEventChannels store event = do
    eventChannels <- readMVar (getEventChannels store)
    mapM_ (atomically . flip writeTChan event) eventChannels

-- | If the store is not open, throw 'StoreClosed'.
assertIsOpen :: Simple -> IO ()
assertIsOpen store = do
    isOpen <- readMVar (getIsOpen store)
    unless isOpen $ CE.throw StoreClosed

----------------------
-- Locations
----------------------

-- | The location of a key's value.
locationValueHash :: Simple -> ValueHash -> FilePath
locationValueHash store hash = locationValues (getBase store) </> BL.unpack hash

-- | The location of a key's record.
locationKey :: Simple -> Key -> FilePath
locationKey store key = locationKeys (getBase store) </> BL.unpack (keyHash key)

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
