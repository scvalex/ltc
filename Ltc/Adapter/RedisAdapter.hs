module Ltc.Adapter.RedisAdapter (
        redisProxyD
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( forM )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Set ( Set )
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
        (reply, stop) <- lift $ case cmd of
            MultiBulk ["PING"] ->
                resply (Status "PONG")
            MultiBulk ["QUIT"] ->
                return (Status "OK", True)
            MultiBulk ["SET", Bulk key, Bulk value] -> do
                _ <- set store (lazy key) (VaString (lazy value))
                resply (Status "OK")
            MultiBulk ["GET", Bulk key] -> do
                mv <- getLatest store (lazy key)
                case mv of
                    Nothing              -> resply Nil
                    Just (VaString s, _) -> resply (Bulk (strict s))
                    _                    -> notAStringReply key
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
                mv <- getLatest store (lazy key)
                resply (maybe (Integer 0) (const (Integer 1)) mv)
            MultiBulk ["APPEND", Bulk key, Bulk value] -> do
                handleAppend key value
            MultiBulk ["STRLEN", Bulk key] -> do
                mv <- getWithDefault (lazy key) ""
                case mv of
                    VaString s -> resply (Integer (fromIntegral (BL.length s)))
                    _          -> notAStringReply key
            MultiBulk ["GETRANGE", Bulk key, Integer start, Integer end] -> do
                handleGetRange key start end
            -- SETRANGE is not supported because Francesco is a pedant.
            MultiBulk ("MGET" : ks) -> do
                messagesToKeys ks handleMGet
            MultiBulk ["SADD", Bulk key, Bulk s] -> do
                handleSAdd key s
            MultiBulk ("SINTER" : ks) ->
                messagesToKeys ks handleSInter
            MultiBulk ["SMEMBERS", key] ->
                messagesToKeys [key] handleSInter
            MultiBulk ["SISMEMBER", Bulk key, Bulk value] -> do
                vs <- getWithDefault (lazy key) (VaStringSet S.empty)
                case vs of
                    VaStringSet s ->
                        resply (Integer (fromIntegral (fromEnum (lazy value `S.member` s))))
                    _ ->
                        resply (toError (printf "key %s is not a string set" (show key)))
            MultiBulk ["SISMEMBER", Bulk key, Integer value] -> do
                vs <- getWithDefault (lazy key) (VaIntSet S.empty)
                case vs of
                    VaIntSet s ->
                        resply (Integer (fromIntegral (fromEnum (value `S.member` s))))
                    _ ->
                        resply (toError (printf "key %s is not an int set" (show key)))
            _ ->
                resply (Error "ERR invalid command")
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
                        ks <- keys store
                        let ks' = filter (\k -> isRightJust (T.execute reg' k))
                                  . map strict
                                  $ S.toList ks
                        resply (MultiBulk (map Bulk ks'))

    handleIncr key delta = do
        mv <- getWithDefault (lazy key) (VaInt 0)
        case mv of
            VaInt n -> do
                _ <- set store (lazy key) (VaInt (n + delta))
                resply (Integer (n + delta))
            _ -> do
                resply (toError (printf "WRONGTYPE key %s does not hold a number" (show key)))

    handleAppend key value = do
        mv <- getWithDefault (lazy key) (VaString "")
        case mv of
            VaString s -> do
                _ <- set store (lazy key) (VaString (BL.append s (lazy value)))
                resply (Integer (fromIntegral (BL.length s + fromIntegral (BS.length value))))
            _ -> do
                notAStringReply key

    handleGetRange key start end = do
        mv <- getWithDefault (lazy key) ""
        case mv of
            VaString s -> do
                let normalize n = if n < 0 then fromIntegral (BL.length s) + n else n
                    start' = fromIntegral (normalize start)
                    end' = fromIntegral (normalize end)
                resply (Bulk (strict (BL.take (end' - start' + 1) (BL.drop start' s))))
            _ ->
                notAStringReply key

    handleMGet ks = do
        values <- forM ks $ \key -> do
            mv <- getLatest store key
            return $ case mv of
                Just (VaString s, _) -> Bulk (strict s)
                _                    -> Nil
        resply (MultiBulk values)

    handleSAdd key s = do
        mv <- getWithDefault (lazy key) (VaStringSet S.empty)
        case mv of
            VaStringSet ss -> do
                let size = S.size ss
                    ss' = S.insert (lazy s) ss
                _ <- set store (lazy key) (VaStringSet ss')
                let size' = S.size ss'
                resply (Integer (fromIntegral (size' - size)))
            _ -> do
                resply . toError
                    $ printf "WRONGTYPE key %s hoes not hold a string set" (show key)

    handleSInter [] = resply (MultiBulk [])
    handleSInter (k:ks) = do
        mv <- getLatest store k
        case mv of
            Nothing -> resply (MultiBulk [])
            Just (VaStringSet s, _) -> do
                mss <- getTypedSets VaStringSet fromVaStringSet ks
                case mss of
                    Nothing -> resply (toError "WRONGTYPE some arguments are not string sets")
                    Just ss -> do
                        let isct = foldl S.intersection s ss
                        resply (MultiBulk (map (Bulk . strict) (S.toList isct)))
            Just (VaIntSet s, _) -> do
                mss <- getTypedSets VaIntSet fromVaIntSet ks
                case mss of
                    Nothing -> resply (toError "WRONGTYPE some arguments are not int sets")
                    Just ss -> do
                        let isct = foldl S.intersection s ss
                        resply (MultiBulk (map Integer (S.toList isct)))
            _ -> resply (toError "WRONGTYPE some arguments are not sets")
      where
        fromVaStringSet (VaStringSet s) = Just s
        fromVaStringSet _               = Nothing

        fromVaIntSet (VaIntSet s) = Just s
        fromVaIntSet _            = Nothing

    -- | Get all the sets associated with the given keys.  Any missing
    -- values default to empty sets.  If any of the sets are not
    -- sets of the right type, return 'Nothing'.
    getTypedSets :: (Set a -> Value) -> (Value -> Maybe (Set a)) -> [Key] -> IO (Maybe [Set a])
    getTypedSets ktr ex ks = do
        sets <- map (maybe (ktr S.empty) fst) <$> forM ks (getLatest store)
        return $ foldl (\mss vs -> ex vs >>= \s -> (s:) <$> mss) (Just []) sets

    -- | Convert a list of 'RedisMessage's to a list of 'Key's.  This
    -- is useful for commands with variable numbers of arguments.
    messagesToKeys :: [RedisMessage] -> ([Key] -> IO (RedisMessage, Bool)) -> IO (RedisMessage, Bool)
    messagesToKeys ks act = go [] ks
      where
        go acc [] = act (reverse acc)
        go acc (Bulk k : kt) = go (lazy k : acc) kt
        go _ _ = resply (toError "WRONGTYPE some arguments are not keys")

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