module Ltc.Adapter.RedisAdapter (
        redisProxyD
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( forM )
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
            MultiBulk ["APPEND", Bulk key, Bulk value] -> do
                handleAppend key value
            MultiBulk ["STRLEN", Bulk key] -> do
                mv <- lift $ getWithDefault (lazy key) ""
                case mv of
                    VaString s -> resply (Integer (fromIntegral (BL.length s)))
                    _          -> notAStringReply key
            MultiBulk ["GETRANGE", Bulk key, Integer start, Integer end] -> do
                handleGetRange key start end
            -- SETRANGE is not supported because Francesco is a pedant.
            MultiBulk ("MGET" : ks) -> do
                handleMGet ks
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
        mv <- lift $ getWithDefault (lazy key) (VaInt 0)
        case mv of
            VaInt n -> do
                _ <- lift $ set store (lazy key) (VaInt (n + delta))
                resply (Integer (n + delta))
            _ -> do
                resply (toError (printf "WRONGTYPE key %s does not hold a number" (show key)))

    handleAppend key value = do
        mv <- lift $ getWithDefault (lazy key) (VaString "")
        case mv of
            VaString s -> do
                _ <- lift $ set store (lazy key) (VaString (BL.append s (lazy value)))
                resply (Integer (fromIntegral (BL.length s + fromIntegral (BS.length value))))
            _ -> do
                notAStringReply key

    handleGetRange key start end = do
        mv <- lift $ getWithDefault (lazy key) ""
        case mv of
            VaString s -> do
                let normalize n = if n < 0 then fromIntegral (BL.length s) + n else n
                    start' = fromIntegral (normalize start)
                    end' = fromIntegral (normalize end)
                resply (Bulk (strict (BL.take (end' - start' + 1) (BL.drop start' s))))
            _ ->
                notAStringReply key

    handleMGet ks = do
        values <- forM ks $ \vkey -> do
            case vkey of
                Bulk key -> do
                    mv <- lift $ getLatest store (lazy key)
                    return $ case mv of
                        Just (VaString s, _) -> Bulk (strict s)
                        _                    -> Nil
                _ -> do
                    return Nil  -- | FIXME Returning nil for bad parameters is a no-no.
        resply (MultiBulk values)

    notAStringReply key =
        resply (toError (printf "WRONGTYPE key %s hoes not hold a string" (show key)))

    -- | Because, usually, we want to not stop the loop.
    resply :: (Monad m) => RedisMessage -> m (RedisMessage, Bool)
    resply msg = return (msg, False)

    toError :: String -> RedisMessage
    toError = Error . strict . BL.pack

    getWithDefault :: Key -> Value -> IO Value
    getWithDefault key def = do
        mv <- getLatest store key
        return (maybe def fst mv)

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