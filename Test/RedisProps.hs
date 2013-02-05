{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative ( (<$>) )
import qualified Control.Exception as CE
import Control.Monad ( when )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ( mempty )
import Ltc.Store
import Network.Socket ( Socket, Family(..), SocketType(..), HostName
                      , socket, sClose, connect, defaultProtocol
                      , AddrInfo(..), getAddrInfo, addrAddress, defaultHints )
import Network.Socket.ByteString ( sendAll, recv )
import qualified Network.Redis as R
import Network.Redis ( RedisMessage, RedisMessage(..) )
import Network.RedisServer ( serveWithPort )
import Test.Common ( cleanEnvironment )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding ( Test )
import Test.QuickCheck
import Text.Printf ( printf )

main :: IO ()
main = defaultMainWithOpts (concat [ msgStructureTests
                                   , endToEndTests
                                   , [testProperty "encodeParse" propEncodeParse]
                                   ]) options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 5000000) }) }

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
endToEndTest request reply = cleanEnvironment ["test-store"] $ do
    store <- open (OpenParameters { location       = "test-store"
                                  , useCompression = False
                                  , nodeName       = "test" })
    let port = 26279
    shutdown <- serveWithPort port store
    sock <- getSocket "localhost" port
    sendAll sock request
    checkRecv sock reply `CE.finally` (shutdown >> close store)
  where
    checkRecv _ leftover | BS.length leftover == 0 =
        return ()
    checkRecv sock leftover = do
        s <- recv sock (BS.length leftover)
        when (BS.null s) $
            assertFailure (printf "reply was not long enough ('%s' left)" (show leftover))
        assertBool (printf "reply does not match: expected '%s'; got '%s'" (show leftover) (show s))
            (BS.isPrefixOf s leftover)
        checkRecv sock (BS.drop (BS.length s) leftover)

endToEndMessages :: [(String, ByteString, ByteString)]
endToEndMessages = [ ("ping", "*1\r\n$4\r\nPING\r\n", "+PONG\r\n")
                   , ("getNE", "*2\r\n$3\r\nGET\r\n$3\r\nfoo\r\n", "$-1\r\n")
                   , ("setGet", "*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$2\r\nba\r\n\
                                \*2\r\n$3\r\nGET\r\n$3\r\nfoo\r\n", "+OK\r\n$2\r\nba\r\n")
                   , ("setSetGet", "*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$2\r\nba\r\n\
                                   \*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$3\r\nbaz\r\n\
                                   \*2\r\n$3\r\nGET\r\n$3\r\nfoo\r\n", "+OK\r\n+OK\r\n$3\r\nbaz\r\n")
                   , ("setSetKeys", "*3\r\n$3\r\nSET\r\n$3\r\nfoo\r\n$2\r\nba\r\n\
                                   \*3\r\n$3\r\nSET\r\n$3\r\nbar\r\n$3\r\nbaz\r\n\
                                   \*2\r\n$4\r\nKEYS\r\n$1\r\n*\r\n",
                                   "+OK\r\n+OK\r\n*2\r\n$3\r\nbar\r\n$3\r\nfoo\r\n")
                   , ("incr", "*2\r\n$4\r\nINCR\r\n$5\r\nmykey\r\n\
                              \*2\r\n$4\r\nINCR\r\n$5\r\nmykey\r\n",":1\r\n:2\r\n")
                   , ("incrby", "*2\r\n$4\r\nINCR\r\n$5\r\nmykey\r\n\
                                \*3\r\n$6\r\nINCRBY\r\n$5\r\nmykey\r\n:5\r\n",":1\r\n:6\r\n")
                   , ("decr", "*3\r\n$6\r\nINCRBY\r\n$5\r\nmykey\r\n:5\r\n\
                              \*2\r\n$4\r\nDECR\r\n$5\r\nmykey\r\n",":5\r\n:4\r\n")
                   , ("decrby", "*3\r\n$6\r\nINCRBY\r\n$5\r\nmykey\r\n:5\r\n\
                                \*3\r\n$6\r\nDECRBY\r\n$5\r\nmykey\r\n:4\r\n",":5\r\n:1\r\n")
                   ]

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

--------------------------------
-- Helpers
--------------------------------

-- | Create a socket connected to the given network address.
getSocket :: HostName -> Int -> IO Socket
getSocket hostname port = do
    addrInfos <- getAddrInfo (Just (defaultHints { addrFamily = AF_INET }))
                             (Just hostname)
                             (Just $ show port)
    CE.bracketOnError
        (socket AF_INET Stream defaultProtocol)
        sClose
        (\s -> do
             connect s (addrAddress $ head addrInfos)
             return s)
