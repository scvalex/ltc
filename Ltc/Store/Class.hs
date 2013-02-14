{-# LANGUAGE TypeFamilies, DeriveDataTypeable, GADTs, FlexibleInstances #-}

module Ltc.Store.Class (
        -- * Store interface
        Store(..),

        -- * Common types
        Key, KeyHash, ValueHash, Version, NodeName,

        -- * Errors
        TypeMismatchError(..), NodeNameMismatchError(..),
        CorruptKeyFileError(..), CorruptValueFileError(..),

        -- * Value types
        Type(..), Value(..), valueString, valueType
    ) where

import Control.Exception ( Exception )
import Data.ByteString.Lazy.Char8 ( ByteString, pack )
import Data.Data ( Data(..), Typeable(..), Fixity(..), DataType, Constr
                 , mkDataType, mkConstr, mkTyConApp, mkTyCon3, constrIndex )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.String ( IsString(..) )
import Data.VectorClock ( VectorClock )
import Language.Sexp ( toSexp, printHum )

----------------------
-- Classes
----------------------

class Store a where
    data OpenParameters a :: *

    open :: OpenParameters a -> IO a
    close :: a -> IO ()

    storeFormat :: a -> String
    storeVersion :: a -> Int

    get :: a -> Key -> Version -> IO (Maybe (Value b))
    getLatest :: a -> Key -> IO (Maybe (Value b, Version))

    keyVersions :: a -> Key -> IO (Maybe [Version])

    set :: a -> Key -> Value b -> IO Version

    keys :: a -> IO (Set Key)

----------------------
-- Types & instances
----------------------

type Key       = ByteString
type KeyHash   = ByteString
type ValueHash = ByteString
type NodeName  = ByteString
type Version   = VectorClock NodeName Int

data BasicValue a where
    VaInt :: Integer -> BasicValue Integer
    VaString :: ByteString -> BasicValue ByteString

instance Typeable (BasicValue a) where
    typeOf (VaInt _) = mkTyConApp (mkTyCon3 "ltc" "Ltc.Store.Class" "VaInt")
                                  [typeOf (undefined :: Integer)]
    typeOf (VaString _) = mkTyConApp (mkTyCon3 "ltc" "Ltc.Store.Class" "VaString")
                                     [typeOf (undefined :: ByteString)]

instance Data (BasicValue Integer) where
    gunfold k z con =
        case constrIndex con of
            1 -> k (z VaInt)
            _ -> error "Data (BasicValue Integer) gunfold"

    dataTypeOf _ = basicValueDataType

    toConstr _ = vaIntConstr

basicValueDataType :: DataType
basicValueDataType = mkDataType "Ltc.Store.Class.BasicValue" [vaIntConstr, vaStringConstr]

vaIntConstr, vaStringConstr :: Constr
vaIntConstr = mkConstr basicValueDataType "VaInt" [] Prefix
vaStringConstr = mkConstr basicValueDataType "VaString" [] Prefix

instance Data (BasicValue ByteString) where
    gunfold k z con =
        case constrIndex con of
            2 -> k (z VaString)
            _ -> error "Data (BasicValue ByteString) gunfold"

    dataTypeOf _ = basicValueDataType

    toConstr _ = vaStringConstr

instance Data (BasicValue a) where
    gunfold = error "Data (BasicValue a) gunfold"

    dataTypeOf = error "Data (BasicValue a) dataTypeOf"

    toConstr = error "Data (BasicValue a) toConstr"

instance IsString (BasicValue ByteString) where
    fromString = VaString . pack

data Value a where
    VaBasic :: BasicValue a -> Value a
    VaSet :: Set (BasicValue a) -> Value (BasicValue a)

instance IsString (Value ByteString) where
    fromString = VaBasic . fromString

-- | The type of a value stored in the store.
data Type = TyString
          | TyInt
          | TyIntSet
          | TyStringSet
          deriving ( Data, Eq, Show, Typeable )

class ValueType a where
    valueType :: a -> Type

instance ValueType (Value Integer) where
    valueType _ = TyInt

instance ValueType (Value ByteString) where
    valueType _ = TyString

instance ValueType (Value (BasicValue Integer)) where
    valueType _ = TyIntSet

instance ValueType (Value (BasicValue ByteString)) where
    valueType _ = TyStringSet

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

----------------------
-- Helpers
----------------------

-- | Get the 'ByteString' representation of a value.
valueString :: Value a -> ByteString
valueString (VaBasic (VaString s)) = s
valueString (VaBasic (VaInt n))    = pack (show n)
valueString (VaSet ss)             = printHum (toSexp (S.toList ss))

-- -- | Get the type of a 'Value'.
-- valueType :: Value a -> Type
-- valueType (VaString _)    = TyString
-- valueType (VaInt _)       = TyInt
-- valueType (VaStringSet _) = TyStringSet
-- valueType (VaIntSet _)    = TyIntSet
