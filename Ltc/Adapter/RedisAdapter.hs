module Ltc.Adapter.RedisAdapter (
        redisProxyD
    ) where

import Control.Applicative ( (<$>) )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import Control.Monad ( unless )
import Control.Proxy
import Ltc.Store
import Network.Redis ( RedisMessage(..) )
import Text.Regex.TDFA
import qualified Text.Regex.TDFA.ByteString as T
import Text.Printf ( printf )

redisProxyD :: (Proxy p, Store s) => s -> () -> Pipe p RedisMessage RedisMessage IO ()
redisProxyD store () = runIdentityP loop
  where
    loop = do
        cmd <- request ()
        (reply, stop) <- case cmd of
            MultiBulk ["PING"] ->
                resply (Status "PONG")
            MultiBulk ["QUIT"] ->
                return (Status "OK", True)
            MultiBulk ["SET", Bulk key, Bulk value] -> do
                _ <- lift $ set store (lazy key) (VaString (lazy value))
                resply (Status "OK")
            MultiBulk ["GET", Bulk key] -> do
                mv <- lift $ getLatest store (lazy key)
                case mv of
                    Nothing     -> resply Nil
                    Just (v, _) -> resply (Bulk (strict (valueString v)))
            MultiBulk ["KEYS", Bulk pat] ->
                handleKeys pat
            MultiBulk ["INCR", Bulk key] ->
                handleIncr key 1
            MultiBulk ["INCRBY", Bulk key, Integer delta] ->
                handleIncr key delta
            MultiBulk ["DECR", Bulk key] ->
                handleIncr key (-1)
            MultiBulk ["DECRBY", Bulk key, Integer delta] ->
                handleIncr key (-delta)
            MultiBulk ["EXISTS", Bulk key] -> do
                mv <- lift $ getLatest store (lazy key)
                resply (maybe (Integer 0) (const (Integer 1)) mv)
            _ ->
                resply (Error "ERR unknown command")
        respond reply
        unless stop loop

    handleKeys pat = do
        case globToRegex (BL.unpack (lazy pat)) of
            Nothing ->
                resply (Error "ERR bad pattern")
            Just reg -> do
                case T.compile defaultCompOpt defaultExecOpt reg of
                    Left err ->
                        resply (toError (printf "ERR bad pattern '%s'" err))
                    Right reg' -> do
                        ks <- lift $ keys store
                        let ks' = filter (\k -> isRightJust (T.execute reg' k))
                                  . map strict
                                  $ S.toList ks
                        resply (MultiBulk (map Bulk ks'))

    handleIncr key delta = do
        mv <- lift $ getLatest store (lazy key)
        case mv of
            Nothing -> do
                _ <- lift $ set store (lazy key) (VaInt delta)
                resply (Integer delta)
            Just (VaInt n, _) -> do
                _ <- lift $ set store (lazy key) (VaInt (n + delta))
                resply (Integer (n + delta))
            Just (_, _) -> do
                resply (toError (printf "WRONGTYPE Key %s does not hold a number" (show key)))

    -- | Because, usually, we want to not stop the loop.
    resply :: (Monad m) => RedisMessage -> m (RedisMessage, Bool)
    resply msg = return (msg, False)

    toError :: String -> RedisMessage
    toError = Error . strict . BL.pack

-- | Make a strict 'ByteString' lazy.
lazy :: ByteString -> BL.ByteString
lazy s = BL.fromChunks [s]

-- | Make a lazy 'ByteString' strict.
strict :: BL.ByteString -> ByteString
strict = BS.concat . BL.toChunks

--------------------------------
-- Helpers
--------------------------------

globToRegex :: (Monad m, Functor m) => String -> m ByteString
globToRegex s = do
    r <- go s
    return (BS.concat ["^", r, "$"])
  where
    go ""         = return ""
    go ('*':cs)   = BS.cons '.' . BS.cons '*' <$> go cs
    go ('?':cs)   = BS.cons '.' <$> go cs
    go ('[':c:cs) = BS.cons '[' . BS.cons c <$> charClass cs
    go (c:cs)     = BS.cons c <$> go cs

    charClass (']':cs) = BS.cons ']' <$> go cs
    charClass (c:cs)   = BS.cons c <$> charClass cs
    charClass _        = fail "unterminated character class"

isRightJust :: Either a (Maybe b) -> Bool
isRightJust (Right (Just _)) = True
isRightJust _                = False