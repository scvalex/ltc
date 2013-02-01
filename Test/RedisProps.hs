{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Network.Redis ( RedisMessage, RedisMessage(..) )
import qualified Network.Redis as R

import Control.Applicative ( (<$>) )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ( mempty )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding ( Test )
import Test.QuickCheck

main :: IO ()
main = defaultMainWithOpts (concat [ msgStructureTests
                                   , endToEndTests
                                   , [testProperty "encodeParse" propEncodeParse]
                                   ]) mempty

--------------------------------
-- Unit tests
--------------------------------

msgStructureTests :: [Test]
msgStructureTests = map (\(n, t, m) -> testCase n (parseTest t m)) structureCommands

structureCommands :: [(String, ByteString, RedisMessage)]
structureCommands =
    [ ("status", "+OK\r\n", Status "OK")
    , ("error", "-ERR unknown command 'foobar'\r\n", Error "ERR unknown command 'foobar'")
    , ("integer", ":1000\r\n", Integer 1000)
    , ("bulk", "$6\r\nfoobar\r\n", Bulk "foobar")
    , ("multibulk", "*5\r\n:1\r\n:2\r\n:3\r\n:4\r\n$6\r\nfoobar\r\n",
       MultiBulk [Integer 1, Integer 2, Integer 3, Integer 4, Bulk "foobar"])
    ]

parseTest :: ByteString -> RedisMessage -> Assertion
parseTest text msg = assertEqual "" msg (R.parseExn text)

endToEndTests :: [Test]
endToEndTests = map (\(n, m, r) -> testCase n (endToEndTest m r)) endToEndMessages

endToEndTest :: ByteString -> ByteString -> Assertion
endToEndTest request reply = undefined

endToEndMessages :: [(String, ByteString, ByteString)]
endToEndMessages = [("ping", "*1\r\n$4\r\nPING\r\n", "+PONG\r\n")]

--------------------------------
-- QuickCheck
--------------------------------

instance Arbitrary ByteString where
    arbitrary = sized $ \n -> do
        BS.pack <$> sequence [ choose (' ', '~') | _ <- [1..n] ]

instance Arbitrary RedisMessage where
    arbitrary = sized $ \sz -> do
        n <- if sz > 1 then choose (1, 5 :: Int) else choose (1, 4)
        case n of
            1 -> Status <$> arbitrary
            2 -> Error <$> arbitrary
            3 -> Integer <$> arbitrary
            4 -> Bulk <$> arbitrary
            5 -> MultiBulk <$> resize (sz `div` 2) arbitrary
            _ -> fail "not a real case of Arbitrary RedisMessage"

propEncodeParse :: RedisMessage -> Bool
propEncodeParse msg = msg == R.parseExn (R.redisEncode msg)
