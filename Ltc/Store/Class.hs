{-# LANGUAGE TypeFamilies, DeriveDataTypeable, EmptyDataDecls #-}

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
import Data.Data ( Data, Typeable )
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

    get :: a -> Key -> Version -> IO (Maybe Value)
    getLatest :: a -> Key -> IO (Maybe (Value, Version))

    keyVersions :: a -> Key -> IO (Maybe [Version])

    set :: a -> Key -> Value -> IO Version

    keys :: a -> IO (Set Key)

----------------------
-- Types & instances
----------------------

type Key       = ByteString
type KeyHash   = ByteString
type ValueHash = ByteString
type NodeName  = ByteString
type Version   = VectorClock NodeName Int

data InhabitedTy

data FunctionTy

-- | The type of a value stored in the store.
data Type a where
    TyString :: Type InhabitedTy
    TyInt    :: Type InhabitedTy
    TySet    :: Type InhabitedTy -> Type FunctionTy
  deriving ( Data, Eq, Show, Typeable )

data Value a where
     VaString :: ByteString -> Value InhabitedTy
     VaInt    :: Integer -> Value InhabitedTy
     VaSet    :: Set 
           | VaInt Integer
           | VaIntSet (Set Integer)
           | VaStringSet (Set ByteString)
           deriving ( Eq, Show )

instance IsString Value where
    fromString = VaString . pack

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
valueString :: Value -> ByteString
valueString (VaString s)     = s
valueString (VaInt n)        = pack (show n)
valueString (VaStringSet ss) = printHum (toSexp (S.toList ss))
valueString (VaIntSet is)    = printHum (toSexp (S.toList is))

-- | Get the type of a 'Value'.
valueType :: Value -> Type
valueType (VaString _)    = TyString
valueType (VaInt _)       = TyInt
valueType (VaStringSet _) = TyStringSet
valueType (VaIntSet _)    = TyIntSet
