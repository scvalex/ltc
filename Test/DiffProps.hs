{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ( (<$>) )
import Data.Monoid ( Monoid(..) )
import Ltc.Store.Diff
import Ltc.Store.Class
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "intDiffWind" propIntDiffWind
       , testProperty "intDiffReverseId" propIntDiffReverseId
       ] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 10000000) }) }

--------------------------------
-- QuickCheck
--------------------------------

instance Arbitrary (Value (Single Integer)) where
    arbitrary = VaInt <$> arbitrary

propIntDiffWind :: Value (Single Integer) -> Value (Single Integer) -> Bool
propIntDiffWind x y = y == applyDiff x (diffFromTo x y)

propIntDiffReverseId :: Value (Single Integer) -> Value (Single Integer) -> Bool
propIntDiffReverseId x y = x == applyDiff y (reverseDiff (diffFromTo x y))
