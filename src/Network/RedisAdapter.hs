{-# LANGUAGE FlexibleContexts, GADTs #-}

-- | This module is the processor for 'RedisMessage's.
module Network.RedisAdapter (
        redisProxyD
    ) where

import Control.Applicative ( (<$>) )
import Control.Exception ( handle )
import Control.Monad ( forM )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Set ( Set )
import qualified Data.Set as S
import Control.Monad ( unless )
import Control.Proxy
import Ltc.Store ( Store(..), Storable, Key(..), Version
                 , TypeMismatchError(..) )
import Network.RedisProtocol ( RedisMessage(..) )
import System.Log.Logger ( debugM, warningM )
import Text.Regex.TDFA
import qualified Text.Regex.TDFA.ByteString as T
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

-- FIXME Redis interface should be stringly-typed

-- | Process 'RedisMessage's in a synchronous fashion.  Note that not all Redis messages
-- are supported, and will "not supported" return errors.
redisProxyD :: (Proxy p, Store s) => s -> () -> Pipe p RedisMessage RedisMessage IO ()
redisProxyD store () = runIdentityP loop
  where
    loop = do
        cmd <- request ()
        lift $ debugM tag "handling command"
        (reply, stop) <- lift $ case cmd of
            MultiBulk ["PING"] ->
                resply (Status "PONG")
            MultiBulk ["QUIT"] ->
                return (Status "OK", True)
            MultiBulk ["SET", Bulk key, Bulk value] -> do
                _ <- set store (mkKey key) (lazy value)
                resply (Status "OK")
            MultiBulk ["GET", Bulk key] -> do
                (mv :: Maybe (BL.ByteString, Version)) <- getLatest store (mkKey key)
                case mv of
                    Nothing     -> resply Nil
                    Just (s, _) -> resply (Bulk (strict s))
                    -- _                    -> notAStringReply key
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
                mv <- getWithDefault (mkKey key) ""
                case mv of
                    s -> resply (Integer (fromIntegral (BL.length s)))
                    -- _ -> notAStringReply key
            MultiBulk ["GETRANGE", Bulk key, Integer start, Integer end] -> do
                handleGetRange key start end
            -- SETRANGE is not supported because Francesco is a pedant.
            MultiBulk ("MGET" : ks) -> do
                messagesToKeys ks handleMGet
            MultiBulk ["SADD", Bulk key, Bulk s] -> do
                handleSAdd key (lazy s)
            -- MultiBulk ["SADD", Bulk key, Integer n] -> do
            --     handleSAdd key s
            MultiBulk ("SINTER" : ks) ->
                messagesToKeys ks handleSInter
            MultiBulk ["SMEMBERS", key] ->
                messagesToKeys [key] handleSInter
            MultiBulk ["SISMEMBER", Bulk key, Bulk value] -> do
                vs <- getWithDefault (mkKey key) (S.empty :: Set BL.ByteString)
                case vs of
                    s ->
                        resply (toRedisBool (lazy value `S.member` s))
                    -- _ ->
                    --     resply (toError (printf "WRONGTYPE key %s is not a string set" (show key)))
            -- MultiBulk ["SISMEMBER", Bulk key, Integer value] -> do
            --     vs <- getWithDefault (mkKey key) (VaIntSet S.empty)
            --     case vs of
            --         VaIntSet s ->
            --             resply (Integer (fromIntegral (fromEnum (value `S.member` s))))
            --         _ ->
            --             resply (toError (printf "WRONGTYPE key %s is not an int set" (show key)))
            MultiBulk ["SCARD", Bulk key] -> do
                (vs :: Maybe (Set BL.ByteString, Version)) <- getLatest store (mkKey key)
                case vs of
                    Nothing ->
                        resply (Integer 0)
                    Just (s, _) ->
                        resply (Integer (fromIntegral (S.size s)))
                    -- Just _ ->
                    --     resply (toError (printf "WRONGTYPE key %s is not a set" (show key)))
            _ -> do
                warningM tag "invalid command"
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
                                  . map (\(Key k) -> strict k)
                                  $ S.toList ks
                        resply (MultiBulk (map Bulk ks'))

    handleIncr key delta = do
        mv <- getWithDefault (mkKey key) (0 :: Integer)
        case mv of
            n -> do
                _ <- set store (mkKey key) (n + delta)
                resply (Integer (n + delta))
            -- _ -> do
            --     resply (toError (printf "WRONGTYPE key %s does not hold a number" (show key)))

    handleAppend key value = do
        mv <- getWithDefault (mkKey key) ""
        case mv of
            s -> do
                _ <- set store (mkKey key) (BL.append s (lazy value))
                resply (Integer (fromIntegral (BL.length s + fromIntegral (BS.length value))))
            -- _ -> do
            --     notAStringReply key

    handleGetRange key start end = do
        mv <- getWithDefault (mkKey key) ""
        case mv of
            s -> do
                let normalize n = if n < 0 then fromIntegral (BL.length s) + n else n
                    start' = fromIntegral (normalize start)
                    end' = fromIntegral (normalize end)
                resply (Bulk (strict (BL.take (end' - start' + 1) (BL.drop start' s))))
            -- _ ->
            --     notAStringReply key

    handleMGet ks = do
        values <- forM ks $ \key -> do
            (mv :: Maybe (BL.ByteString, Version)) <- getLatest store key
            return $ case mv of
                Just (s, _) -> Bulk (strict s)
                _           -> Nil
        resply (MultiBulk values)

    handleSAdd key s = do
        mv <- getWithDefault (mkKey key) (S.empty :: Set BL.ByteString)
        case mv of
            ss -> do
                let size = S.size ss
                    ss' = S.insert s ss
                _ <- set store (mkKey key) ss'
                let size' = S.size ss'
                resply (Integer (fromIntegral (size' - size)))
            -- _ -> do
            --     resply . toError
            --         $ printf "WRONGTYPE key %s hoes not hold a string set" (show key)

    handleSInter [] =
        resply (MultiBulk [])
    handleSInter (k:ks) = do
        (mv :: Maybe (Set BL.ByteString, Version)) <- getLatest store k
        case mv of
            Nothing ->
                resply (MultiBulk [])
            Just (s, _) -> do
                mss <- getStringSets ks
                case mss of
                    Nothing ->
                        resply (toError "WRONGTYPE some arguments are not string sets")
                    Just ss -> do
                        let isct = foldl S.intersection s ss
                        resply (MultiBulk (map (Bulk . strict) (S.toList isct :: [BL.ByteString])))
            -- Just (VaIntSet s, _) -> do
            --     mss <- getTypedSets VaIntSet fromVaIntSet ks
            --     case mss of
            --         Nothing -> resply (toError "WRONGTYPE some arguments are not int sets")
            --         Just ss -> do
            --             let isct = foldl S.intersection s ss
            --             resply (MultiBulk (map Integer (S.toList isct)))
            -- _ ->
            --     resply (toError "WRONGTYPE some arguments are not sets")

    -- | Get all the sets associated with the given keys.  Any missing values default to
    -- empty sets.  If any of the sets are not sets of strings, return 'Nothing'.
    getStringSets :: [Key] -> IO (Maybe [Set BL.ByteString])
    getStringSets ks = do
        handle (\(TypeMismatchError {}) -> return Nothing ) $
            Just . map (maybe S.empty fst) <$> forM ks (getLatest store)

    -- | Convert a list of 'RedisMessage's to a list of 'Key's.  This
    -- is useful for commands with variable numbers of arguments.
    messagesToKeys :: [RedisMessage] -> ([Key] -> IO (RedisMessage, Bool)) -> IO (RedisMessage, Bool)
    messagesToKeys ks act = go [] ks
      where
        go acc []            = act (reverse acc)
        go acc (Bulk k : kt) = go (mkKey k : acc) kt
        go _ _               = resply (toError "WRONGTYPE some arguments are not keys")

    -- notAStringReply key =
    --     resply (toError (printf "WRONGTYPE key %s hoes not hold a string" (show key)))

    -- | Because, usually, we want to not stop the loop.
    resply :: (Monad m) => RedisMessage -> m (RedisMessage, Bool)
    resply msg = return (msg, False)

    toError :: String -> RedisMessage
    toError = Error . strict . BL.pack

    getWithDefault :: (Storable a) => Key -> a -> IO a
    getWithDefault key def = do
        mv <- getLatest store key
        return (maybe def fst mv)

    toRedisBool :: Bool -> RedisMessage
    toRedisBool = Integer . fromIntegral . fromEnum

-- | Make a strict 'ByteString' lazy.
lazy :: BS.ByteString -> BL.ByteString
lazy s = BL.fromChunks [s]

mkKey :: BS.ByteString -> Key
mkKey = Key . lazy

-- | Make a lazy 'ByteString' strict.
strict :: BL.ByteString -> BS.ByteString
strict = BS.concat . BL.toChunks

--------------------------------
-- Helpers
--------------------------------

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

isRightJust :: Either a (Maybe b) -> Bool
isRightJust (Right (Just _)) = True
isRightJust _                = False
