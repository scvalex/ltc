-- | This module is the processor for 'RedisMessage's.  It bridges the Redis interface
-- created by "RedisServer" and a 'Store'.
module Ltc.Network.RedisAdapter (
        redisProxyD
    ) where

import Control.Applicative ( (<$>) )
import Control.Exception ( handle )
import Control.Monad ( forM, unless )
import Control.Proxy ( Proxy, Pipe, runIdentityP, request, respond, lift )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Set ( Set )
import Ltc.Network.RedisProtocol ( RedisMessage(..) )
import Ltc.Store ( Store(..), Storable, SetCmd(..), Key(..), Version
                 , TypeMismatchError(..) )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import System.Log.Logger ( debugM, warningM )
import Text.Printf ( printf )

----------------------
-- Debugging
----------------------

-- | Debugging tag for this module
tag :: String
tag = "RedisAdapter"

----------------------
-- Redis proxy
----------------------

-- | Process 'RedisMessage's in a synchronous fashion.  Note that not all Redis messages
-- are supported, and will "not supported" return errors.
--
-- All Redis entries in a 'Store' will have type 'ByteString', or @Set ByteString@; this
-- matches Redis' own stringly-typed interface.
redisProxyD :: (Proxy p, Store s) => s -> () -> Pipe p RedisMessage RedisMessage IO ()
redisProxyD store () = runIdentityP loop
  where
    loop = do
        cmd <- request ()
        lift $ debugM tag "handling command"
        (reply, stop) <- lift $
            handle (\(TypeMismatchError { expectedType = typ }) ->
                     resply (toError (printf "WRONGTYPE expected: %s" (show typ)))) $ do
                case cmd of
                    MultiBulk ["PING"] ->
                        resply (Status "PONG")
                    MultiBulk ["QUIT"] ->
                        return (Status "OK", True)
                    MultiBulk ["SET", Bulk key, Bulk value] -> do
                        _ <- set store (mkKey key) (lazy value)
                        resply (Status "OK")
                    MultiBulk ["GET", Bulk key] -> do
                        (mv :: Maybe (ByteString, Version)) <- getLatest store (mkKey key)
                        case mv of
                            Nothing     -> resply Nil
                            Just (s, _) -> resply (Bulk (strict s))
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
                    MultiBulk ["APPEND", Bulk key, Bulk value] -> do
                        handleAppend key value
                    MultiBulk ["STRLEN", Bulk key] -> do
                        v <- getWithDefault (mkKey key) ""
                        resply (Integer (fromIntegral (BL.length v)))
                    MultiBulk ["GETRANGE", Bulk key, Integer start, Integer end] -> do
                        handleGetRange key start end
                    -- SETRANGE is not supported because Francesco is a pedant.
                    MultiBulk ("MGET" : ks) -> do
                        messagesToKeys ks handleMGet
                    MultiBulk ("MSET" : ks) -> do
                        messagesToKeyValues ks handleMSet
                    MultiBulk ["SADD", Bulk key, Bulk s] -> do
                        handleSAdd key (lazy s)
                    MultiBulk ["SADD", Bulk key, Integer n] -> do
                        let s = BL.pack (show n)
                        handleSAdd key s
                    MultiBulk ("SINTER" : ks) ->
                        messagesToKeys ks handleSInter
                    MultiBulk ["SMEMBERS", key] ->
                        messagesToKeys [key] handleSInter
                    MultiBulk ["SISMEMBER", Bulk key, Bulk value] -> do
                        checkIsMember key (lazy value)
                    MultiBulk ["SISMEMBER", Bulk key, Integer value] -> do
                        let s = BL.pack (show value)
                        checkIsMember key s
                    MultiBulk ["SCARD", Bulk key] -> do
                        (vs :: Maybe (Set ByteString, Version)) <- getLatest store (mkKey key)
                        case vs of
                            Nothing ->
                                resply (Integer 0)
                            Just (s, _) ->
                                resply (Integer (fromIntegral (S.size s)))
                    command -> do
                        warningM tag (printf "invalid command: %s" (show command))
                        resply (Error "ERR invalid command")
        respond reply
        unless stop loop

    handleKeys pat = do
        case globToRegex (BL.unpack (lazy pat)) of
            Nothing ->
                resply (Error "ERR bad pattern")
            Just reg -> do
                ks <- keys store (BL.unpack (BL.fromStrict reg))
                let ks' = map (\(Key k) -> strict k) (S.toList ks)
                resply (MultiBulk (map Bulk ks'))

    handleIncr key delta = do
        s <- getWithDefault (mkKey key) ("0" :: ByteString)
        case maybeRead s of
            Nothing ->
                resply (toError (printf "WRONGTYPE key %s does not hold an int" (show key)))
            Just n -> do
                _ <- set store (mkKey key) (BL.pack (show (n + delta)))
                resply (Integer (n + delta))

    handleAppend key value = do
        s <- getWithDefault (mkKey key) ""
        _ <- set store (mkKey key) (BL.append s (lazy value))
        resply (Integer (fromIntegral (BL.length s + fromIntegral (BS.length value))))

    handleGetRange key start end = do
        s <- getWithDefault (mkKey key) ""
        let normalize n = if n < 0 then fromIntegral (BL.length s) + n else n
            start' = fromIntegral (normalize start)
            end' = fromIntegral (normalize end)
        resply (Bulk (strict (BL.take (end' - start' + 1) (BL.drop start' s))))

    handleMGet ks = withWriteLock store $ do
        values <- forM ks $ \key -> do
            (mv :: Maybe (ByteString, Version)) <- getLatest store key
            return $ case mv of
                Just (s, _) -> Bulk (strict s)
                _           -> Nil
        resply (MultiBulk values)

    handleMSet kvs = do
        _ <- mset store (map (uncurry SetCmd) kvs)
        resply (Status "OK")

    handleSAdd key s = do
        ss <- getWithDefault (mkKey key) (S.empty :: Set ByteString)
        let size = S.size ss
            ss' = S.insert s ss
        _ <- set store (mkKey key) ss'
        let size' = S.size ss'
        resply (Integer (fromIntegral (size' - size)))

    checkIsMember key value = do
        vs <- getWithDefault (mkKey key) (S.empty :: Set ByteString)
        resply (toRedisBool (value `S.member` vs))

    handleSInter [] =
        resply (MultiBulk [])
    handleSInter (k:ks) = do
        (mv :: Maybe (Set ByteString, Version)) <- getLatest store k
        case mv of
            Nothing ->
                resply (MultiBulk [])
            Just (s, _) -> do
                mss <- getStringSets ks
                case mss of
                    Nothing ->
                        resply (toError "WRONGTYPE some arguments are not sets")
                    Just ss -> do
                        let isct = foldl S.intersection s ss
                        resply (MultiBulk (map (Bulk . strict) (S.toList isct :: [ByteString])))

    -- | Get all the sets associated with the given keys.  Any missing values default to
    -- empty sets.  If any of the sets are not sets of strings, return 'Nothing'.
    getStringSets :: [Key] -> IO (Maybe [Set ByteString])
    getStringSets ks = do
        handle (\(TypeMismatchError {}) -> return Nothing ) $
            Just . map (maybe S.empty fst) <$> forM ks (getLatest store)

    -- | Convert a list of 'RedisMessage's to a list of 'Key's.  This is useful for
    -- commands with variable numbers of arguments.
    messagesToKeys :: [RedisMessage]
                   -> ([Key] -> IO (RedisMessage, Bool))
                   -> IO (RedisMessage, Bool)
    messagesToKeys ks act = go [] ks
      where
        go acc []            = act (reverse acc)
        go acc (Bulk k : kt) = go (mkKey k : acc) kt
        go _ _               = resply (toError "WRONGTYPE some arguments are not keys")

    messagesToKeyValues :: [RedisMessage]
                        -> ([(Key, ByteString)] -> IO (RedisMessage, Bool))
                        -> IO (RedisMessage, Bool)
    messagesToKeyValues ks act = go [] ks
      where
        go acc []                        = act (reverse acc)
        go _ [_]                         = resply (toError "ERR last key had no value")
        go acc (Bulk k : Bulk v : kt)    = go ((mkKey k, lazy v) : acc) kt
        go acc (Bulk k : Integer v : kt) = go ((mkKey k, BL.pack (show v)) : acc) kt
        go _ _                           = resply (toError "WRONGTYPE some arguments where not keys")

    -- | Because, usually, we want to not stop the loop.
    resply :: (Monad m) => RedisMessage -> m (RedisMessage, Bool)
    resply msg = return (msg, False)

    -- | Get the value of a key, or return the given default if the key does no exist.
    getWithDefault :: (Storable a) => Key -> a -> IO a
    getWithDefault key def = do
        mv <- getLatest store key
        return (maybe def fst mv)

--------------------------------
-- Helpers
--------------------------------

-- | Convert a 'String' to an 'Error' 'RedisMessage'.
toError :: String -> RedisMessage
toError = Error . strict . BL.pack

-- | Convert a 'Bool' to an 'Integer' 'RedisMessage'
toRedisBool :: Bool -> RedisMessage
toRedisBool = Integer . fromIntegral . fromEnum

-- | Make a strict 'ByteString' lazy.
lazy :: BS.ByteString -> ByteString
lazy s = BL.fromChunks [s]

-- | Make a 'Key' from a strict 'BS.ByteString'.
mkKey :: BS.ByteString -> Key
mkKey = Key . lazy

-- | Make a lazy 'ByteString' strict.
strict :: ByteString -> BS.ByteString
strict = BS.concat . BL.toChunks

-- | Try to read a value; return 'Nothing' if the parse fails.
maybeRead :: (Read a) => ByteString -> Maybe a
maybeRead bs =
    let s = BL.unpack bs in
    case readsPrec 0 s of
        [(x, "")] -> Just x
        _         -> Nothing

globToRegex :: (Monad m, Functor m) => String -> m BS.ByteString
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
