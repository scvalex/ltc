{-# LANGUAGE EmptyDataDecls, GADTs, DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ltc.Store.Types (
        -- * Key Types
        Key(..), KeyHash,

        -- * Value types
        Value(..), Single, Collection, ValueHash,

        -- * Node types
        NodeName,

        -- * Version types
        Version
    ) where

import Control.Applicative ( (<$>) )
import Data.Aeson ( ToJSON )
import Data.ByteString.Lazy.Char8 ( ByteString, pack )
import Data.Maybe ( fromJust )
import Data.Serialize ( Serialize(..) )
import Data.Set ( Set )
import Data.String ( IsString(..) )
import Data.VectorClock ( VectorClock )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexp(..), Sexpable(..), printMach, parseExn )
import Text.Printf ( printf )

----------------------
-- Types & instances
----------------------

newtype Key = Key ByteString
            deriving ( Eq, Generic, Ord, Show )

instance Sexpable Key

instance ToJSON Key

instance Serialize Key

instance IsString Key where
    fromString = Key . pack

-- FIXME Make KeyHash, ValueHjash, NodeName, and Version abstract types
type KeyHash = ByteString
type ValueHash = ByteString
type NodeName = ByteString

type Version = VectorClock NodeName Int

instance Serialize Version

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

instance (Sexpable (Value a)) => Serialize (Value a) where
    put d = put (printMach (toSexp d))

    get = fromJust . fromSexp . head . parseExn <$> get
