{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ( (<$>) )
import Data.Monoid ( Monoid(..) )
import Ltc.Store.Diff
import Ltc.Store.Class
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import System.Random ( Random )
import qualified Data.Set as S

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "intDiffWind" propIntDiffWind
       , testProperty "intSetDiffWind" propIntSetDiffWind
       , testProperty "intDiffReverseId" propIntDiffReverseId
       , testProperty "intSetDiffReverseId" propIntSetDiffReverseId
       ] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 10000000) }) }

--------------------------------
-- QuickCheck
--------------------------------

instance Arbitrary (Value (Single Integer)) where
    arbitrary = VaInt <$> arbitrary

newtype SmallInt = SmallInt { unSmallInt :: Integer }
                 deriving ( Num, Random )

instance Arbitrary SmallInt where
    arbitrary = choose (1, 10)

instance Arbitrary (Value (Collection Integer)) where
    arbitrary = VaSet . S.fromList . map (VaInt . unSmallInt) <$> arbitrary

propIntDiffWind :: Value (Single Integer) -> Value (Single Integer) -> Bool
propIntDiffWind x y = y == applyDiff x (diffFromTo x y)

propIntSetDiffWind :: Value (Collection Integer) -> Value (Collection Integer) -> Bool
propIntSetDiffWind xs ys = ys == applyDiff xs (diffFromTo xs ys)

propIntDiffReverseId :: Value (Single Integer) -> Value (Single Integer) -> Bool
propIntDiffReverseId x y = x == applyDiff y (reverseDiff (diffFromTo x y))

propIntSetDiffReverseId :: Value (Collection Integer) -> Value (Collection Integer) -> Bool
propIntSetDiffReverseId xs ys = xs == applyDiff ys (reverseDiff (diffFromTo xs ys))
