{-# LANGUAGE DeriveDataTypeable #-}

module Network.Redis (
        RedisMessage(..), redisParser,
        ParseException(..), parse, parseExn
    ) where

import Control.Applicative ( (<$>), (<*), many )
import Control.Monad ( replicateM )
import Control.Exception ( Exception )
import qualified Control.Exception as CE
import Data.Attoparsec.ByteString.Char8 ( Parser, IResult(..), char, notChar, (<?>) )
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.Attoparsec.Combinator ( choice )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.String ( fromString )
import Data.Typeable ( Typeable )

data ParseException = ParseException String ByteString
                    deriving ( Show, Typeable )

instance Exception ParseException

-- | A Redis message.  This is used both for sending commands to the
-- server, and for receiving replies.
data RedisMessage = Status ByteString
                  | Error ByteString
                  | Integer Integer
                  | Bulk ByteString
                  | MultiBulk [RedisMessage]
                  deriving ( Eq, Show )

-- | Parse a Redis command from a lazy 'ByteString'.  If the parse was
-- successful, @Right msg@ is returned; otherwise, @Left (errorMsg,
-- leftover)@ is returned.
parse :: ByteString -> Either (String, ByteString) RedisMessage
parse = resultToEither . AC.parse redisParser
  where
    resultToEither (Fail leftover _ctxs reason) =
        Left (reason, leftover)
    resultToEither (Partial _) =
        Left ("not enough input", "")
    resultToEither (Done leftover sexps) =
        if BS.null leftover
        then Right sexps
        else Left ("garbage at end", leftover)

-- | A variant of 'parse' that throws a 'ParseException' if the parse
-- fails.
parseExn :: ByteString -> RedisMessage
parseExn text =
    case parse text of
        Left (reason, leftover) -> CE.throw (ParseException reason leftover)
        Right cmd               -> cmd

-- | A parser for commands on Redis' wire protocol.
--
-- See <http://redis.io/topics/protocol> for details.
redisParser :: Parser RedisMessage
redisParser =
    choice [ char '+' >> statusParser <?> "status"
           , char '-' >> errorParser <?> "error"
           , char ':' >> integerParser <?> "integer"
           , char '$' >> bulkParser <?> "bulk"
           , char '*' >> multiBulkParser <?> "multibulk" ]
  where
    statusParser = Status . fromString <$> crlfTerminatedString
    errorParser = Error . fromString <$> crlfTerminatedString
    integerParser = Integer <$> crlfTerminatedInteger
    bulkParser = do
        n <- crlfTerminatedInteger
        Bulk <$> AC.take (fromIntegral n) <* crlf
    multiBulkParser = do
        n <- crlfTerminatedInteger
        MultiBulk <$> replicateM (fromIntegral n) redisParser

    crlf = char '\r' >> char '\n'
    crlfTerminatedString = many (notChar '\r') <* crlf
    crlfTerminatedInteger = read <$> crlfTerminatedString
