{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ( (<$>) )
import Control.Monad ( replicateM )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Monoid ( Monoid(..) )
import Data.Set ( Set )
import Ltc.Diff ( Diffable(..) )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import System.Random ( Random )
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "intDiffWind" (propDiffWind :: DiffWindTest Integer)
       , testProperty "stringDiffWind" (propDiffWind :: DiffWindTest ByteString)
       , testProperty "intSetDiffWind" (propDiffWind :: DiffWindTest (Set Integer))
       , testProperty "intDiffReverseId" (propDiffReverseId :: DiffReverseIdTest Integer)
       , testProperty "stringDiffReverseId" (propDiffReverseId :: DiffReverseIdTest ByteString)
       , testProperty "intSetDiffReverseId" (propDiffReverseId :: DiffReverseIdTest (Set Integer))
       ] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 10000000) }) }

--------------------------------
-- QuickCheck
--------------------------------

instance Arbitrary ByteString where
    arbitrary = sized $ \n -> do
        BL.pack <$> replicateM n (choose ('a', 'd'))

newtype SmallInt = SmallInt { unSmallInt :: Integer }
                 deriving ( Num, Random )

instance Arbitrary SmallInt where
    arbitrary = choose (1, 10)

instance Arbitrary (Set Integer) where
    arbitrary = S.fromList . map unSmallInt <$> arbitrary

type DiffWindTest a = a -> a -> Bool

propDiffWind :: (Eq a, Diffable a) => DiffWindTest a
propDiffWind x y = y == applyDiff x (diffFromTo x y)

type DiffReverseIdTest a = a -> a -> Bool

propDiffReverseId :: (Eq a, Diffable a) => DiffReverseIdTest a
propDiffReverseId x y = x == applyDiff y (reverseDiff (diffFromTo x y))
