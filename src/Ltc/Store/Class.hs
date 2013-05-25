{-# LANGUAGE TypeFamilies, DeriveDataTypeable, GADTs, FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ltc.Store.Class (
        -- * Store interface
        Store(..), withStore, keyVersionsExn, getExn, getLatestExn,

        -- * Common types
        Key(..), KeyHash, ValueHash, Version, NodeName,

        -- * Errors
        TypeMismatchError(..), NodeNameMismatchError(..),
        CorruptKeyFileError(..), CorruptValueFileError(..),
        CorruptStoreError(..),

        -- * Value types
        Value(..), Single, Collection, Type(..), ValueString(..), ValueType(..)
    ) where

import Control.Applicative ( (<$>) )
import Control.Exception ( Exception )
import Data.ByteString.Lazy.Char8 ( ByteString, pack, unpack )
import Data.Set ( Set )
import Data.String ( IsString(..) )
import Data.Typeable ( Typeable )
import Data.VectorClock ( VectorClock )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexp(..), Sexpable(..), printHum, parseMaybe )
import Ltc.Store.EventHandler ( EventHandler )
import qualified Control.Exception as CE
import qualified Data.Set as S
import Text.Printf ( printf )

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

    -- | Get the value associated with the given key at the given version.  Note that you
    -- should know the type of the value before in order to use this function.
    get :: (ValueString (Value b)) => a -> Key ->  Version -> IO (Maybe (Value b))

    -- | Get the latest value associated with the given key.  Note that you should know
    -- the type of the value in order to use this function.
    getLatest :: (ValueString (Value b)) => a -> Key -> IO (Maybe (Value b, Version))

    -- | Get all versions of the values associated with the given key, most-recent-first.
    keyVersions :: a -> Key -> IO (Maybe [Version])

    -- | Get the type of the values associated with a key.  A key cannot be associated
    -- with values of different types.
    keyType :: a -> Key -> IO (Maybe Type)

    -- | Set the value associated with a key.
    set :: (ValueString (Value b), ValueType (Value b)) => a -> Key -> Value b -> IO Version

    -- | Get all the keys stored in the store.  Note that using this is probably racey
    -- because the set of keys may change before it is used.
    keys :: a -> IO (Set Key)

    -- | Add an event handler to a store.  Multiple event handlers may be associated with
    -- a store at any one time.
    addEventHandler :: a -> EventHandler -> IO ()

-- | Open a store, run the given action, and close the store.  The store is cleanly closed
-- even if the action throws an exception; the exception is rethrown afterwards.
withStore :: (Store s) => OpenParameters s -> (s -> IO a) -> IO a
withStore params act = do
    store <- open params
    CE.handle (\(e :: CE.SomeException) -> do
                    close store
                    CE.throw e)
        (act store)

keyVersionsExn :: (Store s) => s -> Key -> IO [Version]
keyVersionsExn store key = do
    mvsns <- keyVersions store key
    case mvsns of
        Nothing   -> CE.throw (NoVersionsFor key)
        Just vsns -> return vsns

getExn :: (Store s, ValueString (Value a)) => s -> Key -> Version -> IO (Value a)
getExn store key vsn = do
    mv <- get store key vsn
    case mv of
        Nothing -> CE.throw (NoValueFor key vsn)
        Just v  -> return v

getLatestExn :: (Store s, ValueString (Value a)) => s -> Key -> IO (Value a, Version)
getLatestExn store key = do
    mvv <- getLatest store key
    case mvv of
        Nothing -> CE.throw (NoValueForLatest key)
        Just vv -> return vv

----------------------
-- Types & instances
----------------------

newtype Key = Key ByteString
            deriving ( Eq, Generic, Ord, Show )

instance Sexpable Key

instance IsString Key where
    fromString = Key . pack

type KeyHash   = ByteString
type ValueHash = ByteString
type NodeName  = ByteString

type Version   = VectorClock NodeName Int

instance (Sexpable a, Sexpable b) => Sexpable (VectorClock a b)

data Single a

data Collection a

data Value a where
    VaInt :: Integer -> Value (Single Integer)
    VaString :: ByteString -> Value (Single ByteString)
    VaSet :: Set (Value (Single b)) -> Value (Collection b)

instance (Show a) => Show (Value (Single a)) where
    show (VaInt n) = show n
    show (VaString s) = show s

instance (Show a) => Show (Value (Collection a)) where
    show (VaSet s) = printf "VaSet (%s)" (show s)

instance Eq (Value (Single a)) where
    (VaInt n1) == (VaInt n2)       = n1 == n2
    (VaString s1) == (VaString s2) = s1 == s2
    _ == _                         = False

instance Eq (Value (Collection a)) where
    (VaSet s1) == (VaSet s2) = s1 == s2

instance (Ord a) => Ord (Value (Single a)) where
    (VaInt n1) `compare` (VaInt n2) = n1 `compare` n2
    (VaString s1) `compare` (VaString s2) = s1 `compare` s2
    _ `compare` _ = error "Impossible case in Ord (Value (Single a))"

instance Sexpable (Value (Single Integer)) where
    toSexp (VaInt n) = List ["VaInt", toSexp n]
    fromSexp (List ["VaInt", s]) = VaInt <$> fromSexp s
    fromSexp _                   = fail "fromSexp Value (Single Integer)"

instance Sexpable (Value (Single ByteString)) where
    toSexp (VaString s) = List ["VaString", toSexp s]
    fromSexp (List ["VaString", s]) = VaString <$> fromSexp s
    fromSexp _                      = fail "fromSexp Value (Single ByteString)"

instance (Sexpable (Value (Single a)), Ord (Value (Single a)))
         => Sexpable (Value (Collection a)) where
    toSexp (VaSet s) = List ["VaSet", toSexp s]
    fromSexp (List ["VaSet", s]) = VaSet <$> fromSexp s
    fromSexp _                   = fail "fromSexp Value (Collection a)"

----------------------
-- Value Helpers
----------------------

-- | A concrete representation of the types supported by LTc.
data Type = SingleString
          | SingleInteger
          | CollectionString
          | CollectionInteger
          deriving ( Eq, Generic, Show )

instance Sexpable Type

class ValueType a where
    valueType :: a -> Type

instance ValueType (Value (Single Integer)) where
    valueType _ = SingleInteger

instance ValueType (Value (Single ByteString)) where
    valueType _ = SingleString

instance ValueType (Value (Collection Integer)) where
    valueType _ = CollectionInteger

instance ValueType (Value (Collection ByteString)) where
    valueType _ = CollectionString

class ValueString a where
    -- | Get the 'ByteString' representation of a value.
    valueString :: a -> ByteString

    -- | Get a value from its 'ByteString' representation.
    unValueString :: ByteString -> Maybe a

instance ValueString (Value (Single Integer)) where
    valueString (VaInt n) = pack (show n)

    unValueString s =
        case readsPrec 1 (unpack s) of
            [(n, _)] -> Just (VaInt n)
            _        -> Nothing

instance ValueString (Value (Single ByteString)) where
    valueString (VaString s) = s

    unValueString = Just . VaString

instance (ValueString (Value (Single a)), Ord (Value (Single a)))
         => ValueString (Value (Collection a)) where
    valueString (VaSet ss) =
        printHum (toSexp (S.map valueString ss))

    unValueString s =
        case parseMaybe s of
            Nothing ->
                Nothing         -- no sexp
            Just [sexp] -> do
                case fromSexp sexp of
                    Nothing ->
                        Nothing -- invalid sexp
                    Just (ss :: [ByteString]) ->
                        VaSet . S.fromList <$>
                        (mapM unValueString ss :: Maybe [Value (Single a)])
            Just _ ->
                Nothing         -- more than one sexp

----------------------
-- Exceptions
----------------------

data TypeMismatchError = TypeMismatchError { expectedType :: Type
                                           , foundType    :: Type
                                           }
                       deriving ( Show, Typeable )

instance Exception TypeMismatchError

data NodeNameMismatchError = NodeNameMismatchError { storeName     :: ByteString
                                                   , requestedName :: ByteString
                                                   }
                           deriving ( Show, Typeable )

instance Exception NodeNameMismatchError

data CorruptKeyFileError = CorruptKeyFileError { keyFilePath :: FilePath
                                               , ckfReason   :: String
                                               }
                         deriving ( Show, Typeable )

instance Exception CorruptKeyFileError

data CorruptValueFileError = CorruptValueFileError { valueFilePath :: FilePath
                                                   , cvfReason     :: String
                                                   }
                           deriving ( Show, Typeable )

instance Exception CorruptValueFileError

data CorruptStoreError = CorruptStoreError { csReason :: String }
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
