{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ( (<$>) )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Map ( Map )
import Data.Monoid ( mempty )
import Data.Set ( Set )
import Data.String ( fromString )
import Network.NodeProtocol ( NodeMessage(..) )
import Ltc.Diff ( Diff, EditScript, EditAction )
import Ltc.Store ( Version, Key )
import Ltc.Store.VersionControl ( DiffPack, KeyHistory )
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Framework
import Test.Framework.Providers.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series

main :: IO ()
main = defaultMainWithOpts
       [ -- testProperty "encode-decode-node-message/id" encodeDecodeNodeMessageId
       ] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 10000000) }) }

--------------------------------
-- SmallCheck
--------------------------------

instance (Monad m) => Serial m NodeMessage

instance (Monad m) => Serial m DiffPack

instance (Monad m) => Serial m Version

instance (Monad m) => Serial m (Map Key KeyHistory) where
    series = M.fromList <$> series

instance (Monad m) => Serial m (Key, KeyHistory)

instance (Monad m) => Serial m KeyHistory

instance (Monad m) => Serial m ByteString where
    series = fromString <$> series

instance (Monad m) => Serial m Key

instance (Monad m) => Serial m (Diff Integer)

instance (Monad m) => Serial m (Diff ByteString)

instance (Monad m) => Serial m EditScript

instance (Monad m) => Serial m EditAction

instance (Monad m) => Serial m (Diff (Set Integer))

instance (Monad m) => Serial m (Diff (Set ByteString))

instance (Monad m, Serial m a, Ord a) => Serial m (Set a) where
    series = S.fromList <$> series

encodeDecodeNodeMessageId :: NodeMessage -> Property IO
encodeDecodeNodeMessageId _ = undefined
