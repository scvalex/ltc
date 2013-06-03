{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Monoid ( mempty )
import Network.NodeProtocol ( NodeEnvelope(..), NodeMessage(..) )
import Test.Framework
import Test.Framework.Providers.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "encode-decode-node-message/id" encodeDecodeNodeMessageId
       ] mempty

--------------------------------
-- SmallCheck
--------------------------------

instance (Monad m) => Serial m NodeMessage

encodeDecodeNodeMessageId :: NodeMessage -> Property IO
encodeDecodeNodeMessageId _ = undefined
