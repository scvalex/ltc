{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ( (<$>) )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Function ( on )
import Data.Map ( Map )
import Data.Monoid ( mempty )
import Data.Set ( Set )
import Data.String ( fromString )
import Ltc.Diff ( Diff, EditScript, EditAction )
import Ltc.Network.Interface.Null ( NullInterface, NetworkLocation(..) )
import Ltc.Network.NodeProtocol ( NodeEnvelope(..), NodeMessage(..) )
import Ltc.Network.RedisProtocol ( RedisMessage(..) )
import Ltc.Store ( Version, Key )
import Ltc.Store.VersionControl ( DiffPack, KeyHistory )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Ltc.Network.NodeProtocol as P
import qualified Ltc.Network.RedisProtocol as R
import Test.Framework
import Test.Framework.Providers.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series ( Serial(..), cons2 )
import Test.SmallCheck.Drivers ( test )

main :: IO ()
main = defaultMainWithOpts
       [ testProperty "encode-decode-node-envelope/id" propEncodeDecodeNodeEnvelopeId
       , testProperty "encode-parse-redis/id" propEncodeParseRedisId
       ] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 1000000) }) }

--------------------------------
-- Node protocol
--------------------------------

instance (Monad m) => Serial m (NodeEnvelope NullInterface) where
    series = cons2 NodeEnvelope

instance (Monad m) => Serial m (NetworkLocation NullInterface) where
    series = return NullLocation

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

instance Eq (NodeEnvelope NullInterface) where
    (==) = (==) `on` getEnvelopeMessage

propEncodeDecodeNodeEnvelopeId :: NodeEnvelope NullInterface -> Property IO
propEncodeDecodeNodeEnvelopeId envelope =
    let s = P.encode envelope in
    -- WARNING: This seems to hang if envelope is weird (if it has undefined fields,
    -- etc.).
    test (P.decode s == Just envelope)

--------------------------------
-- Redis protocol
--------------------------------

instance (Monad m) => Serial m RedisMessage

instance (Monad m) => Serial m BS.ByteString where
    series = fromString <$> series

propEncodeParseRedisId :: RedisMessage -> Property IO
propEncodeParseRedisId msg =
    test (msg == R.parseExn (R.redisEncode msg))
