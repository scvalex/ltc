{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Monoid ( mempty )
import Network.NodeProtocol ( NodeEnvelope(..), NodeMessage(..) )
import Test.Framework
import Test.Framework.Providers.SmallCheck
import Test.SmallCheck

main :: IO ()
main = defaultMainWithOpts
       [
       ] mempty

--------------------------------
-- SmallCheck
--------------------------------
