module Main where

import Network.Redis ( RedisMessage, RedisMessage(..) )
import qualified Network.Redis as R

import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Monoid ( mempty )
import Test.Framework
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding ( Test )
-- import Test.QuickCheck

main :: IO ()
main = defaultMainWithOpts msgStructureTests mempty

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
