{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Common ( cleanEnvironment, cleanEnvironmentP, testParameters )
import Control.Applicative ( (<$>), (<*>) )
import qualified Control.Exception as CE
import Control.Monad ( replicateM, when )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Foldable ( foldlM )
import qualified Data.Map as M
import Data.Monoid ( mempty )
import Ltc.Store ( Store(..) )
import Ltc.Store.Simple ( OpenParameters(..) )
import Network.Socket ( Socket, Family(..), SocketType(..)
                      , socket, sClose, connect, defaultProtocol
                      , AddrInfo(..), getAddrInfo, addrAddress, defaultHints )
import Network.Socket.ByteString ( sendAll, recv )
import qualified Network.Redis as R
import Network.Redis ( RedisMessage, RedisMessage(..) )
import Network.RedisServer ( Hostname, Port, serveWithPort )
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding ( Test )
import Test.QuickCheck
import Test.QuickCheck.Monadic as QCM
import Text.Printf ( printf )

main :: IO ()
main = defaultMainWithOpts (concat [ msgStructureTests
                                   , endToEndBinaryTests
                                   , endToEndTests
                                   , [ testProperty "encodeParse" propEncodeParse
                                     , testProperty "numericDance" propNumericDance ]
                                   ]) options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 15000000) }) }

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

endToEndBinaryTests :: [Test]
endToEndBinaryTests =
    map (\(n, m, r) -> testCase n (endToEndBinaryTest m r)) endToEndBinaryMessages

endToEndTests :: [Test]
endToEndTests =
    map (\(n, ms, rs) -> testCase n (endToEndBinaryTest (prepare ms) (prepare rs)))
        endToEndMessages
  where
    prepare = BS.concat . map R.redisEncode

endToEndBinaryTest :: ByteString -> ByteString -> Assertion
endToEndBinaryTest request reply = cleanEnvironment ["redis-store"] $ do
    store <- open testParameters { location = "redis-store" }
    let port = 26279
    shutdown <- serveWithPort port store
    sock <- getSocket "localhost" port
    sendAll sock request
    checkRecv sock reply `CE.finally` (shutdown >> close store)
    sClose sock
    shutdown
    close store
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

endToEndBinaryMessages :: [(String, ByteString, ByteString)]
endToEndBinaryMessages =
    [ ("ping", "*1\r\n$4\r\nPING\r\n", "+PONG\r\n")
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

endToEndMessages :: [(String, [RedisMessage], [RedisMessage])]
endToEndMessages =
    [ ("get",
       [ MultiBulk ["GET", "mykey"]
       , MultiBulk ["INCR", "mykey"]
       , MultiBulk ["GET", "mykey"] ],
       -- FIXME GET on an int key should fail with WRONGTYPE
       [ Nil, Integer 1, "1" ])
    , ("append",
       [ MultiBulk ["APPEND", "mykey", "Hello"]
       , MultiBulk ["APPEND", "mykey", " World"]
       , MultiBulk ["GET", "mykey"] ],
       [ Integer 5, Integer 11, "Hello World" ])
    , ("strlen",
       [ MultiBulk ["SET", "mykey", "Hello world"]
       , MultiBulk ["STRLEN", "mykey"]
       , MultiBulk ["STRLEN", "nonexisting"] ],
       [ Status "OK", Integer 11, Integer 0 ])
    , ("getrange",
       [ MultiBulk ["SET", "mykey", "This is a string"]
       , MultiBulk ["GETRANGE", "mykey", Integer 0, Integer 3]
       , MultiBulk ["GETRANGE", "mykey", Integer (-3), Integer (-1)]
       , MultiBulk ["GETRANGE", "mykey", Integer 0, Integer (-1)]
       , MultiBulk ["GETRANGE", "mykey", Integer 10, Integer 100]],
       [ Status "OK", "This", "ing", "This is a string", "string" ])
    , ("mget",
       [ MultiBulk ["SET", "key1", "Hello"]
       , MultiBulk ["SET", "key2", "World"]
       , MultiBulk ["MGET", "key1", "key2", "nonexisting"] ],
       [ Status "OK", Status "OK", MultiBulk [ "Hello", "World", Nil ] ])
    , ("sadd/smembers",
       [ MultiBulk ["SADD", "myset", "Hello"]
       , MultiBulk ["SADD", "myset", "World"]
       , MultiBulk ["SADD", "myset", "World"]
       , MultiBulk ["SMEMBERS", "myset"] ],
       [ Integer 1, Integer 1, Integer 0, MultiBulk ["Hello", "World"] ])
    , ("sinter",
       [ MultiBulk ["SADD", "key1", "a"]
       , MultiBulk ["SADD", "key1", "b"]
       , MultiBulk ["SADD", "key1", "c"]
       , MultiBulk ["SADD", "key2", "c"]
       , MultiBulk ["SADD", "key2", "d"]
       , MultiBulk ["SADD", "key2", "e"]
       , MultiBulk ["SINTER", "key1", "key2"] ],
       [Integer 1, Integer 1, Integer 1, Integer 1, Integer 1, Integer 1, MultiBulk ["c"]])
    , ("sismember",
       [ MultiBulk ["SADD", "myset", "one"]
       , MultiBulk ["SISMEMBER", "myset", "one"]
       , MultiBulk ["SISMEMBER", "myset", "two"] ],
       [Integer 1, Integer 1, Integer 0])
    , ("scard",
       [ MultiBulk ["SADD", "myset", "Hello"]
       , MultiBulk ["SADD", "myset", "World"]
       , MultiBulk ["SCARD", "myset"] ],
       [Integer 1, Integer 1, Integer 2])
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

type TestKey = ByteString

data NumericRedisMessage = Incr TestKey
                         | IncrBy TestKey Integer
                         | Decr TestKey
                         | DecrBy TestKey Integer
                         deriving ( Show )

newtype NumericRedisMessages = NRMs { unNRMs :: [NumericRedisMessage] }
                             deriving ( Show )

instance Arbitrary NumericRedisMessages where
    arbitrary = sized $ \n -> do
        let kn = ceiling (sqrt (fromIntegral n :: Double)) :: Int
        ks <- replicateM kn arbitrary
        NRMs <$> replicateM n (makeNumericMessage ks)
      where
        makeNumericMessage ks = do
            n <- choose (1, 4 :: Int)
            let key = elements ks
            case n of
                1 -> Incr <$> key
                2 -> IncrBy <$> key <*> arbitrary
                3 -> Decr <$> key
                4 -> DecrBy <$> key <*> arbitrary
                _ -> fail "unknown case in 'Arbitrary Command'"

propEncodeParse :: RedisMessage -> Bool
propEncodeParse msg = msg == R.parseExn (R.redisEncode msg)

propNumericDance :: NumericRedisMessages -> Property
propNumericDance (NRMs msgs) = monadicIO $ cleanEnvironmentP ["redis-store"] $ do
    store <- run $ open testParameters { location = "redis-store" }
    let port = 26279
    shutdown <- run $ serveWithPort port store
    sock <- run $ getSocket "localhost" port
    _ <- foldlM (runCmd sock) M.empty msgs
    run $ sClose sock
    run $ shutdown
    run $ close store
  where
    runCmd sock kns (Incr key) = do
        run $ sendMessage sock (MultiBulk ["INCR", Bulk key])
        r <- run $ receiveMessage sock
        runCmdWithDelta kns key 1 r
    runCmd sock kns (Decr key) = do
        run $ sendMessage sock (MultiBulk ["DECR", Bulk key])
        r <- run $ receiveMessage sock
        runCmdWithDelta kns key (-1) r
    runCmd sock kns (IncrBy key delta) = do
        run $ sendMessage sock (MultiBulk ["INCRBY", Bulk key, Integer delta])
        r <- run $ receiveMessage sock
        runCmdWithDelta kns key delta r
    runCmd sock kns (DecrBy key delta) = do
        run $ sendMessage sock (MultiBulk ["DECRBY", Bulk key, Integer delta])
        r <- run $ receiveMessage sock
        runCmdWithDelta kns key (-delta) r

    runCmdWithDelta kns key delta r = do
        let (Integer n) = M.findWithDefault (Integer 0) key kns
        QCM.assert (Integer (n + delta) == r)
        return (M.insert key (Integer (n + delta)) kns)

--------------------------------
-- Helpers
--------------------------------

-- | Create a socket connected to the given network address.
getSocket :: Hostname -> Port -> IO Socket
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

-- | Send a Redis message to a server.
sendMessage :: Socket -> RedisMessage -> IO ()
sendMessage sock msg = sendAll sock (R.redisEncode msg)

-- | Receive a Redis message from a server in a stupid way.
receiveMessage :: Socket -> IO RedisMessage
receiveMessage sock = do
    R.parseExn <$> recv sock 4096 -- FIXME We should handle leftovers somehow.
