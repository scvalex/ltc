{-# LANGUAGE TypeFamilies, DeriveDataTypeable, GADTs, FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls, StandaloneDeriving, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ltc.Store.Class (
        -- * Store interface
        Store(..),

        -- * Common types
        Key, KeyHash, ValueHash, Version, NodeName,

        -- * Errors
        TypeMismatchError(..), NodeNameMismatchError(..),
        CorruptKeyFileError(..), CorruptValueFileError(..),

        -- * Value types
        Value(..), Single, Collection, Type(..), ValueString(..), ValueType(..)
    ) where

import Control.Applicative ( (<$>) )
import Control.Exception ( Exception )
import Data.ByteString.Lazy.Char8 ( ByteString, pack, unpack )
import GHC.Generics ( Generic )
import Data.Typeable ( Typeable )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.VectorClock ( VectorClock )
import Language.Sexp ( Sexpable(..), printHum, parseMaybe )

----------------------
-- Classes
----------------------

class Store a where
    data OpenParameters a :: *

    open :: OpenParameters a -> IO a
    close :: a -> IO ()

    storeFormat :: a -> String
    storeVersion :: a -> Int

    get :: (ValueString (Value b)) => a -> Key ->  Version -> IO (Maybe (Value b))
    getLatest :: (ValueString (Value b)) => a -> Key -> IO (Maybe (Value b, Version))

    keyVersions :: a -> Key -> IO (Maybe [Version])

    set :: (ValueString (Value b), ValueType (Value b)) => a -> Key -> Value b -> IO Version

    keys :: a -> IO (Set Key)

----------------------
-- Types & instances
----------------------

type Key       = ByteString
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

instance Eq (Value (Single a)) where
    (VaInt n1) == (VaInt n2)       = n1 == n2
    (VaString s1) == (VaString s2) = s1 == s2
    _ == _                         = False

instance Ord (Value (Single Integer)) where
    (VaInt n1) `compare` (VaInt n2) = n1 `compare` n2

instance Ord (Value (Single ByteString)) where
    (VaString s1) `compare` (VaString s2) = s1 `compare` s2

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
        printHum (toSexp (map valueString (S.toList ss)))

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
