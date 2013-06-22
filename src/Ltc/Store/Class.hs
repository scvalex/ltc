{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, TypeFamilies #-}

module Ltc.Store.Class (
        -- * Store interface
        Store(..), SetCmd(..), withStore, keyVersionsExn, getExn, getLatestExn,

        -- * Errors
        TypeMismatchError(..), NodeNameMismatchError(..),
        CorruptKeyFileError(..), CorruptValueFileError(..),
        CorruptStoreError(..), StoreClosed(..),
        NoVersionsFor(..), NoValueFor(..), NoValueForLatest(..),
        CorruptChangesetError(..), CorruptChangelogError(..),

        module Ltc.Store.Types
    ) where

-- Re-exported module
import Ltc.Store.Types

import Control.Exception ( Exception )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Set ( Set )
import Data.Typeable ( Typeable, TypeRep )
import Ltc.Store.Event ( EventChannel )
import Ltc.Changeset ( Changeset )
import qualified Control.Exception as CE

----------------------
-- Classes
----------------------

class Store a where
    data OpenParameters a :: *

    -- | Open a 'Store' with the given parameters.
    open :: OpenParameters a -> IO a

    -- | Close a 'Store'.
    close :: a -> IO ()

    -- | A string used to tag the format of the on-disk files.
    storeFormat :: a -> String

    -- | The current version of the files in the format.
    storeVersion :: a -> Int

    -- | Get the value associated with the given key at the given version.  If the
    -- requested version does not exist, return 'Nothing'.  Note that you should know the
    -- type of the value before in order to use this function; if the type on disk is not
    -- the same as the requested type, throw 'TypeMismatchError'.
    get :: (Storable b) => a -> Key -> Version -> IO (Maybe b)

    -- | Get the latest value associated with the given key.  Note that you should know
    -- the type of the value in order to use this function.
    getLatest :: (Storable b) => a -> Key -> IO (Maybe (b, Version))

    -- | Get all versions of the values associated with the given key, most-recent-first.
    keyVersions :: a -> Key -> IO (Maybe [Version])

    -- | Get all the 'Changeset's not before the given version.
    --
    -- @
    --    v1
    --   /  \
    -- v2    v3
    --   \  /
    --    v4
    -- @
    --
    -- @
    --     changesetsNotBefore v2 == [v3, v4]
    -- @
    changesetsNotBefore :: a -> Version -> IO [Changeset]

    -- | Get the type of the values associated with a key.  A key cannot be associated
    -- with values of different types.
    keyType :: a -> Key -> IO (Maybe Type)

    -- | Set the value associated with a key.  If 'close' was already called on this
    -- store, throw 'StoreClosed'.
    set :: (Storable b) => a -> Key -> b -> IO Version

    -- | Atomically set the values associated with the keys.  By atomic we mean that
    -- either all the values are set, or none other.  This function is called 'mset'
    -- because it's a multi-set.  If 'close' was already called on this store, throw
    -- 'StoreClosed'.
    mset :: a -> [SetCmd] -> IO Version

    -- | Acquire the write lock, execute the given action, and release the write lock.
    -- Since writes to the store cannot happen while the lock is acquired, this is an
    -- effective way to atomically get multiple values from the store.  Note that if you
    -- try to write while the write lock is acquired, you will block indefinitely.
    withWriteLock :: a -> IO b -> IO b

    -- FIXME Store.keys should take a regexp pattern.
    -- | Get all the keys stored in the store.  Note that using this is probably racey
    -- because the set of keys may change before it is used.
    keys :: a -> IO (Set Key)

    -- | Add an event channel to the store.  Events will be written are written to it as
    -- they happen.
    addEventChannel :: a -> EventChannel -> IO ()

data SetCmd = forall a. (Storable a) => SetCmd Key a

-- | Open a store, run the given action, and close the store.  The store is cleanly closed
-- even if the action throws an exception; the exception is rethrown afterwards.
withStore :: (Store s) => OpenParameters s -> (s -> IO a) -> IO a
withStore params act = do
    store <- open params
    CE.handle (\(e :: CE.SomeException) -> do
                    close store
                    CE.throw e)
        (act store)

-- | Get all versions of the values associated with the given key, most-recent-first.  If
-- the key does not exist, throw 'NoVersionsFor'.
keyVersionsExn :: (Store s) => s -> Key -> IO [Version]
keyVersionsExn store key = do
    mvsns <- keyVersions store key
    case mvsns of
        Nothing   -> CE.throw (NoVersionsFor key)
        Just vsns -> return vsns

-- | Get the value associated with the given key at the given version.  If the key does
-- not exist, throw 'NoValueFor'.
getExn :: (Store s, Storable a) => s -> Key -> Version -> IO a
getExn store key vsn = do
    mv <- get store key vsn
    case mv of
        Nothing -> CE.throw (NoValueFor key vsn)
        Just v  -> return v

-- | Get the latest value associated with the given key.  If the key does not exist, throw
-- 'NoValueForLatest'.
getLatestExn :: (Store s, Storable a) => s -> Key -> IO (a, Version)
getLatestExn store key = do
    mvv <- getLatest store key
    case mvv of
        Nothing -> CE.throw (NoValueForLatest key)
        Just vv -> return vv

----------------------
-- Exceptions
----------------------

data TypeMismatchError = TypeMismatchError
    { expectedType :: TypeRep
    , foundType    :: TypeRep
    } deriving ( Show, Typeable )

instance Exception TypeMismatchError

data NodeNameMismatchError = NodeNameMismatchError
    { storeName     :: ByteString
    , requestedName :: ByteString
    } deriving ( Show, Typeable )

instance Exception NodeNameMismatchError

data CorruptKeyFileError = CorruptKeyFileError
    { keyFilePath :: FilePath
    , ckfReason   :: String
    } deriving ( Show, Typeable )

instance Exception CorruptKeyFileError

data CorruptValueFileError = CorruptValueFileError
    { valueFilePath :: FilePath
    , cvfReason     :: String
    } deriving ( Show, Typeable )

instance Exception CorruptValueFileError

data CorruptStoreError = CorruptStoreError String
                       deriving ( Show, Typeable )

instance Exception CorruptStoreError

data NoVersionsFor = NoVersionsFor Key
                   deriving ( Show, Typeable )

instance Exception NoVersionsFor

data NoValueFor = NoValueFor Key Version
                deriving ( Show, Typeable )

instance Exception NoValueFor

data NoValueForLatest = NoValueForLatest Key
                      deriving ( Show, Typeable )

instance Exception NoValueForLatest

data StoreClosed = StoreClosed
                   deriving ( Show, Typeable )

instance Exception StoreClosed

data CorruptChangesetError = CorruptChangesetError
    { changesetPath :: FilePath
    , ccsReason     :: String
    } deriving ( Show, Typeable )

instance Exception CorruptChangesetError

data CorruptChangelogError = CorruptChangelogError String
                           deriving ( Show, Typeable )

instance Exception CorruptChangelogError
