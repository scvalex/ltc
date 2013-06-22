{-# LANGUAGE TypeFamilies, TupleSections, DeriveGeneric #-}
{-# Language FlexibleContexts, FlexibleInstances #-}

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
-- ├── clock
-- ├── changelog
-- ├── nodeName
-- ├── clean-shutdown
-- ├── tmp/
-- ├── keys/
-- ├── changesets/
-- └── values/
-- @
--
-- @DB_DIR/format@ contains a single string identifying the format of
-- the key-value store.  In our case, this is "simple".
--
-- @DB_DIR/version@ contains the version of the key-value store.  This
-- is used to upgrade on-disk files.
--
-- @DB_DIR/clock@ is the latest version clock seen by the store.  This is probably also
-- the version of the latest value written to disk, but this is not necessary.
--
-- @DB_DIR/changelog@ is a list of all the versions and associated 'Changeset's of this
-- store.
--
-- @DB_DIR/clean-shutdown@ is only present if 1) the store is closed, and 2) the store was
-- shutdown cleanly.
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
-- @DB_DIR/changesets@ is a directory that contains a file for each change made to the
-- key-value store.
--
-- @DB_DIR/values@ is a directory that contains a file for each value
-- present in the key-value store.  The values in these files may be
-- gzipped.  These files are referenced by files in the @DB_DIR/keys@
-- directory.
module Ltc.Store.Simple (
        Simple, OpenParameters(..)
    ) where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( MVar, newMVar
                          , modifyMVar, modifyMVar_, readMVar, withMVar )
import Control.Concurrent.STM ( atomically, writeTChan )
import Control.Monad ( when, unless, forM )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Default ( Default(..) )
import Data.Digest.Pure.SHA ( sha1, showDigest, integerDigest )
import Data.Foldable ( foldlM )
import Data.Set ( Set )
import Data.Typeable ( TypeRep, typeOf )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable(..), parse, parseExn, printHum )
import Ltc.Changeset ( Changeset(..)
                     , changesFromList
                     , wireDiffForKeyExn, wireDiffFromTo, diffFromWireDiff )
import Ltc.Diff ( Diffable(..) )
import Ltc.Store.Class ( Store(..), SetCmd(..)
                       , Key(..), KeyHash
                       , NodeName
                       , Storable, ValueHash
                       , StoreClosed(..), CorruptValueFileError(..), NodeNameMismatchError(..)
                       , TypeMismatchError(..), CorruptStoreError(..), CorruptKeyFileError(..)
                       , Version, ChangesetHash
                       , CorruptChangesetError(..), CorruptChangelogError(..) )
import Ltc.Store.Event ( EventChannel, Event(..), SetEvent(..) )
import qualified Codec.Compression.GZip as Z
import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import qualified Data.VectorClock as VC
import System.Directory ( createDirectory, doesFileExist, doesDirectoryExist
                        , renameFile, getDirectoryContents, removeFile )
import System.FilePath ( (</>) )
import System.IO ( hClose, openBinaryTempFile )
import System.Log.Logger ( debugM, warningM )
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
storeVsn = 7

----------------------
-- Types
----------------------

data Simple = Simple
    { getBase           :: FilePath
    , getUseCompression :: Bool
    , getNodeName       :: NodeName
    , getEventChannels  :: MVar [EventChannel]
    , getIsOpen         :: MVar Bool
    , getClock          :: MVar Version
    , getLock           :: MVar ()
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
    , getValueType :: TypeRep
    , getTip       :: KeyVersion
    , getHistory   :: [ChangesetHash]
    } deriving ( Generic )

instance Sexpable KeyRecord

data Changelog = Changelog [(Version, ChangesetHash)]
               deriving ( Generic )

instance Sexpable Changelog

----------------------
-- Store interface
----------------------

instance Store Simple where
    -- | Store configuration.
    data OpenParameters Simple = SimpleParameters
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

    changesetsAfter store version = doChangesetsAfter store version

    keyType store key = doKeyType store key

    set store key value = doSet store key value

    mset store kvs = doMSet store kvs

    withWriteLock store act = lockStore store act

    keys store = doKeys store

    addEventChannel store eventChannel = doAddEventChannel store eventChannel

instance Default (OpenParameters Simple) where
    def = SimpleParameters { location        = "ltc-store"
                           , useCompression  = False
                           , nodeName        = "my-node"
                           , createIfMissing = True
                           , forceOpen       = False
                           }

doOpen :: OpenParameters Simple -> IO Simple
doOpen params = do
    debugM tag (printf "open store '%s'" (location params))

    -- Make sure that the store exists on disk.
    storeExists <- doesDirectoryExist (location params)
    when (not storeExists) $ do
        if createIfMissing params
            then initStore params
            else CE.throw (CorruptStoreError "missing")

    -- Check that the requested node name is the same as the one on disk.
    nn <- BL.readFile (locationNodeName (location params))
    when (not (forceOpen params) && nn /= nodeName params) $
        CE.throw (NodeNameMismatchError { requestedName = nodeName params
                                        , storeName     = nn })

    -- Check that the store's format version is the same as the one in this executable.
    vsn <- BL.readFile (locationVersion (location params))
    when (vsn /= BL.pack (show storeVsn)) $
        CE.throw (CorruptStoreError "different version")

    cleanShutdown <- doesFileExist (locationCleanShutdown (location params))
    if cleanShutdown
        then removeFile (locationCleanShutdown (location params))
        else doRecover

    clock <- newMVar =<< readClockExn (locationClock (location params))

    eventChannels <- newMVar []
    isOpen <- newMVar True
    lock <- newMVar ()

    return (Simple { getBase           = location params
                   , getUseCompression = useCompression params
                   , getNodeName       = nodeName params
                   , getEventChannels  = eventChannels
                   , getIsOpen         = isOpen
                   , getClock          = clock
                   , getLock           = lock
                   })
  where
    doRecover = do
        warningM tag "recovering store"

doClose :: Simple -> IO ()
doClose store = do
    debugM tag (printf "close store '%s'" (getBase store))
    modifyMVar_ (getIsOpen store) (const (return False))
    writeFile (locationCleanShutdown (getBase store)) ""
    writeEventChannels store CloseEvent

doGet :: forall a. (Storable a) => Simple -> Key -> Version -> IO (Maybe a)
doGet store key version = do
    debugM tag (printf "get %s" (show key))
    writeEventChannels store getEvent
    CE.handle (\(exn :: CE.IOException) -> do
                CE.throw (CorruptKeyFileError { keyFilePath = locationKey store key
                                              , ckfReason   = show exn })) $ do
        withKeyRecord (locationKey store key) $ \kr -> do
            when (getValueType kr /= typeOf (undefined :: a)) $
                CE.throw (TypeMismatchError { expectedType = typeOf (undefined :: a)
                                            , foundType    = getValueType kr })

            -- Read the tip value
            let valueFile = locationValueHash store (getValueHash (getTip kr))
            s <- (if getUseCompression store then Z.decompress else id)
                 <$> BL.readFile valueFile
            let val = case valueFromBinary s of
                    Nothing -> CE.throw (CorruptValueFileError {
                                              valueFilePath = valueFile,
                                              cvfReason     = "unparsable" })
                    Just v -> v

            -- Walk back through the history looking for the earliest version that was
            -- before or equal to the given version.
            Just <$> findVersion val (getHistory kr)
  where
    getEvent =
        let Key k = key
        in GetEvent { eventKey  = key
                    , keyDigest = fromInteger (integerDigest (sha1 k))
                    }

    valueFromBinary :: ByteString -> Maybe a
    valueFromBinary s = do
        case parse s of
            Left (err, _) -> fail err
            Right [sexp]  -> fromSexp sexp
            Right _       -> fail "wrong number of sexps"

    findVersion :: a -> [ChangesetHash] -> IO a
    findVersion val [] =
        return val
    findVersion val (chash : history) = do
        changeset <- readChangesetExn (locationChangesetHash store chash)
        case changeset of
            Update {} -> do
                if not (getAfterVersion changeset `VC.causes` version)
                    then do
                        let wireDiff = wireDiffForKeyExn (getChanges changeset) key
                        -- The only way for 'diffFromWireDiff' to fail is if the type is
                        -- wrong, but it cannot be at this point in the execution.
                        let Just diff = diffFromWireDiff wireDiff
                        let rdiff = reverseDiff diff
                        let val' = applyDiff val rdiff
                        findVersion val' history
                    else do
                        return val
            Merge {} ->
                -- FIXME Implement getting history past merges.
                error "walking back through merges not supported"

doGetLatest :: (Storable a) => Simple -> Key -> IO (Maybe (a, Version))
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
        Just <$> forM (getHistory kr) (\chash -> do
            changeset <- readChangesetExn (locationChangesetHash store chash)
            return (getAfterVersion changeset))

doChangesetsAfter :: Simple -> Version -> IO [Changeset]
doChangesetsAfter store version = do
    Changelog changesets <- readChangelogExn store
    let changesetPaths =
            map (locationChangesetHash store) $
            map snd $
            takeWhile (\(otherVersion, _) -> not (otherVersion `VC.causes` version))
                      changesets
    changesets' <- mapM readChangesetExn changesetPaths
    return (reverse changesets')

doKeyType :: Simple -> Key -> IO (Maybe TypeRep)
doKeyType store key = do
    debugM tag (printf "keyType %s" (show key))
    withKeyRecord (locationKey store key) $ \kr -> do
        return (Just (getValueType kr))

doSet :: (Storable a) => Simple -> Key -> a -> IO Version
doSet store key value = doMSet store [SetCmd key value]

doMSet :: Simple -> [SetCmd] -> IO Version
doMSet store cmds = lockStore store $ do
    assertIsOpen store

    -- Log the set everywhere
    debugM tag (printf "mset %s" (show (map (\(SetCmd key _) -> key) cmds)))

    -- Increment and save the version clock.  It's ok to increment the clock
    -- superfluously, so we can be interrupted here.
    let nn = getNodeName store
    (clock, clock') <- modifyMVar (getClock store) $ \oldClock -> do
        let Just incrementedClock = VC.inc nn oldClock
        return (incrementedClock, (oldClock, incrementedClock))
    atomicWriteClock store (locationClock (getBase store)) clock'

    -- Write the values.  It's ok to write extra values, so we can be interrupted here.
    (vals, vhashes) <- unzip <$> forM cmds (\(SetCmd _ value) -> do
        let vhash = valueHash value
            val = valueToString value
        atomicWriteFile store (locationValueHash store vhash)
            ((if getUseCompression store then Z.compress else id) val)
        return (val, vhash))

    -- Read the key records (the store is locked so there's no risk of them being updated
    -- by something else).
    mkrs <- forM (zip cmds vhashes) $ \(cmd@(SetCmd key _), vhash) -> do
        mkr <- readKeyRecordExn (locationKey store key)
        return (mkr, cmd, vhash)

    -- Compute the 'Changeset' from the store state to the new one (the store is locked,
    -- so values can't be updated by something else while we're here).  Having superfluous
    -- changesets lying around is not a problem.
    changes <- changesFromList <$> forM cmds (\(SetCmd key newVal) -> do
        mOldVal <- doGetLatest store key
        let oldVal = maybe def fst mOldVal
        return (key, wireDiffFromTo oldVal newVal))
    let changeset = Update { getBeforeUpdateVersion = clock
                           , getAfterVersion        = clock'
                           , getChanges             = changes }
    let serializedChangeset = printHum (toSexp changeset)
    let chash = BL.pack (showDigest (sha1 serializedChangeset))
    atomicWriteFile store (locationChangesetHash store chash) serializedChangeset

    -- Update the key records, or create new ones if they are missing.
    krs' <- forM mkrs $ \(mkrOld, cmd@(SetCmd key value), vhash) -> do
        let tip = KeyVersion { getVersion   = clock'
                             , getValueHash = vhash
                             }
        case mkrOld of
            Nothing -> return $
                (cmd, KR { getKeyName   = key
                         , getValueType = typeOf value
                         , getTip       = tip
                         , getHistory   = [chash]
                         })
            Just krOld -> do
                when (getValueType krOld /= typeOf value) $ do
                    CE.throw (TypeMismatchError { expectedType = getValueType krOld
                                                , foundType    = typeOf value })
                return $ (cmd, krOld { getTip     = tip
                                     , getHistory = chash : getHistory krOld
                                     })
    Changelog changesets <- readChangelogExn store
    let changelog' = Changelog ((clock', chash) : changesets)
    atomicWriteFiles store $
        (locationChangelog (getBase store), printHum (toSexp changelog')) :
        (flip map krs' $ \(SetCmd key _, kr) ->
          (locationKey store key, printHum (toSexp kr)))

    writeEventChannels store (msetEvent vals)
    return clock'
  where
    msetEvent vals =
        MSetEvent (flip map (zip cmds vals) $ \(SetCmd key _, val) ->
                    let Key k = key
                    in SetEvent { setKey       = key
                                , setKeyDigest = fromInteger (integerDigest (sha1 k))
                                , valueDigest  = fromInteger (integerDigest (sha1 val))
                                })

    -- | The hash of a value.  This hash is used as the filename under which the value is
    -- stored in the @values/@ folder.
    valueHash :: (Storable a) => a -> ValueHash
    valueHash = BL.pack . showDigest . sha1 . valueToString

    valueToString :: (Storable a) => a -> ByteString
    valueToString = printHum . toSexp

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

-- | Wrapper around 'readKeyRecordExn'.
withKeyRecord :: FilePath -> (KeyRecord -> IO (Maybe a)) -> IO (Maybe a)
withKeyRecord path f = do
    mkr <- readKeyRecordExn path
    case mkr of
        Nothing -> return Nothing
        Just kr -> f kr

-- FIXME parseOrError should be part of sexp.
-- | Read a S-Expression-encoded value from a file, or invoke the given handler if an
-- error occurs.
parseWithHandler :: (Sexpable a) => FilePath -> (String -> IO a) -> IO a
parseWithHandler path handleErr = do
    text <- BL.readFile path
    case parse text of
        Left err ->
            handleErr (show err)
        Right [s] ->
            case fromSexp s of
                Nothing -> handleErr "invalid sexp"
                Just kr -> return kr
        Right _ ->
            handleErr "multiple sexps"

-- | Read a key record from disk.  If the key doesn't exist, return
-- 'Nothing'.  If the key record is corrupt, fail.
readKeyRecordExn :: FilePath -> IO (Maybe KeyRecord)
readKeyRecordExn path = do
    keyExists <- doesFileExist path
    if keyExists
          then do
              Just <$> parseWithHandler path (\reason -> do
                  CE.throw (CorruptKeyFileError { keyFilePath = path
                                                , ckfReason  = reason }))
          else return Nothing

-- | Read a 'Changeset' from disk.
readChangesetExn :: FilePath -> IO Changeset
readChangesetExn path = do
    parseWithHandler path $ \reason -> do
        CE.throw (CorruptChangesetError { changesetPath = path
                                        , ccsReason     = reason })

-- | Read the store's 'Changelog'.
readChangelogExn :: Simple -> IO Changelog
readChangelogExn store = do
    parseWithHandler (locationChangelog (getBase store)) $ \reason -> do
        CE.throw (CorruptChangelogError reason)

-- | Write the given 'ByteString' to the file atomically.  Overwrite any previous content.
-- The 'Simple' reference is needed in order to find the temporary directory (we can't use
-- @/tmp@ because that may be on a different partition and 'renameFile' doesn't work in
-- that case).
atomicWriteFile :: Simple -> FilePath -> ByteString -> IO ()
atomicWriteFile store path content = do
    (tempFile, handle) <- openBinaryTempFile (locationTemporary (getBase store)) "ltc"
    BL.hPut handle content `CE.finally` hClose handle
    renameFile tempFile path

-- | Write a version clock to disk atomically.
atomicWriteClock :: Simple -> FilePath -> Version -> IO ()
atomicWriteClock store path clock =
    atomicWriteFile store path (printHum (toSexp clock))

-- | Write the given 'ByteString's to the given files atomically.  See 'atomicWriteFile'
-- for details.
atomicWriteFiles :: Simple -> [(FilePath, ByteString)] -> IO ()
atomicWriteFiles store pcs = do
    -- FIXME Make 'atomicWriteFiles' atomic.
    mapM_ (uncurry (atomicWriteFile store)) pcs

-- | Read a version clock from disk.  Throw an exception if it is missing or if it is
-- malformed.
readClockExn :: FilePath -> IO Version
readClockExn path = fromSexp . head . parseExn =<< BL.readFile path

-- | Create the initial layout for the store at the given directory
-- base.
initStore :: OpenParameters Simple -> IO ()
initStore params = do
    debugM tag "initStore"
    let base = location params
    createDirectory base
    writeFile (locationFormat base) formatString
    writeFile (locationVersion base) (show storeVsn)
    let initialClock = VC.insert (nodeName params) (0 :: Int) VC.empty
    BL.writeFile (locationClock base) (printHum (toSexp initialClock))
    BL.writeFile (locationChangelog base) (printHum (toSexp (Changelog [])))
    BL.writeFile (locationNodeName base) (nodeName params)
    writeFile (locationCleanShutdown base) ""
    createDirectory (locationTemporary base)
    createDirectory (locationValues base)
    createDirectory (locationKeys base)
    createDirectory (locationChangesets base)

-- | The hash of a key.  This hash is used as the filename under which
-- the key is stored in the @keys/@ folder.
keyHash :: Key -> KeyHash
keyHash (Key k) = BL.pack (showDigest (sha1 k))

-- | Write event to all event channels
writeEventChannels :: Simple -> Event -> IO ()
writeEventChannels store event = do
    eventChannels <- readMVar (getEventChannels store)
    -- FIXME Is there any way to signal that a TChan is "closed"?
    mapM_ (atomically . flip writeTChan event) eventChannels

-- | If the store is not open, throw 'StoreClosed'.
assertIsOpen :: Simple -> IO ()
assertIsOpen store = do
    isOpen <- readMVar (getIsOpen store)
    unless isOpen $ CE.throw StoreClosed

-- | Lock the store for the duration of the given action: only one such action can execute
-- at any time.
lockStore :: Simple -> IO a -> IO a
lockStore store act = withMVar (getLock store) (const act)

----------------------
-- Locations
----------------------

-- | The location of a key's value.
locationValueHash :: Simple -> ValueHash -> FilePath
locationValueHash store hash = locationValues (getBase store) </> BL.unpack hash

-- | The location of a 'Changeset'.
locationChangesetHash :: Simple -> ChangesetHash -> FilePath
locationChangesetHash store hash = locationChangesets (getBase store) </> BL.unpack hash

-- | The location of a key's record.
locationKey :: Simple -> Key -> FilePath
locationKey store key = locationKeys (getBase store) </> BL.unpack (keyHash key)

locationFormat :: FilePath -> FilePath
locationFormat base = base </> "format"

locationVersion :: FilePath -> FilePath
locationVersion base = base </> "version"

locationClock :: FilePath -> FilePath
locationClock base = base </> "clock"

locationCleanShutdown :: FilePath -> FilePath
locationCleanShutdown base = base </> "clean-shutdown"

locationTemporary :: FilePath -> FilePath
locationTemporary base = base </> "tmp"

locationValues :: FilePath -> FilePath
locationValues base = base </> "values"

locationKeys :: FilePath -> FilePath
locationKeys base = base </> "keys"

locationChangesets :: FilePath -> FilePath
locationChangesets base = base </> "changesets"

locationChangelog :: FilePath -> FilePath
locationChangelog base = base </> "changelog"

locationNodeName :: FilePath -> FilePath
locationNodeName base = base </> "nodeName"
