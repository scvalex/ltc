{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ( (<$>) )
import Control.Monad ( replicateM )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Monoid ( Monoid(..) )
import Ltc.Store.Diff ( Diffable(..) )
import Ltc.Store.Class
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import System.Random ( Random )
import qualified Data.Set as S
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "intDiffWind" (propDiffWind :: DiffWindTest (Single Integer))
       , testProperty "stringDiffWind" (propDiffWind :: DiffWindTest (Single ByteString))
       , testProperty "intSetDiffWind" (propDiffWind :: DiffWindTest (Collection Integer))
       , testProperty "intDiffReverseId" (propDiffReverseId :: DiffReverseIdTest (Single Integer))
       , testProperty "stringDiffReverseId" (propDiffReverseId :: DiffReverseIdTest (Single ByteString))
       , testProperty "intSetDiffReverseId" (propDiffReverseId :: DiffReverseIdTest (Collection Integer))
       ] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 10000000) }) }

--------------------------------
-- QuickCheck
--------------------------------

instance Arbitrary (Value (Single Integer)) where
    arbitrary = VaInt <$> arbitrary

instance Arbitrary (Value (Single ByteString)) where
    arbitrary = sized $ \n -> do
        VaString . BL.pack <$> replicateM n (choose ('a', 'd'))

newtype SmallInt = SmallInt { unSmallInt :: Integer }
                 deriving ( Num, Random )

instance Arbitrary SmallInt where
    arbitrary = choose (1, 10)

instance Arbitrary (Value (Collection Integer)) where
    arbitrary = VaSet . S.fromList . map (VaInt . unSmallInt) <$> arbitrary

type DiffWindTest a = Value a -> Value a -> Bool

propDiffWind :: (Eq (Value a), Diffable a) => DiffWindTest a
propDiffWind x y = y == applyDiff x (diffFromTo x y)

type DiffReverseIdTest a = Value a -> Value a -> Bool

propDiffReverseId :: (Eq (Value a), Diffable a) => DiffReverseIdTest a
propDiffReverseId x y = x == applyDiff y (reverseDiff (diffFromTo x y))
