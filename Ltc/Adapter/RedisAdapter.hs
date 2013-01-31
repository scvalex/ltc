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
            return (Status "PONG", False)
        MultiBulk ["QUIT"] ->
            return (Status "OK", True)
        MultiBulk ["SET", Bulk key, Bulk value] -> do
            _ <- lift $ set store (lazy key) (lazy value)
            return (Status "OK", False)
        MultiBulk ["GET", Bulk key] -> do
            mv <- lift $ getLatest store (lazy key)
            case mv of
                Nothing     -> return (Nil, False)
                Just (v, _) -> return (Bulk (strict v), False)
        MultiBulk ["KEYS", Bulk pat] -> do
            case globToRegex (BL.unpack (lazy pat)) of
                Nothing -> return (Error "ERR bad pattern", False)
                Just reg -> do
                    case T.compile defaultCompOpt defaultExecOpt reg of
                        Left err ->
                            return (Error (strict (BL.pack (printf "ERR bad pattern '%s'" err))), False)
                        Right reg' -> do
                            ks <- lift $ keys store
                            let ks' = filter (\k -> isRightJust (T.execute reg' k))
                                      . map strict
                                      $ S.toList ks
                            return (MultiBulk (map Bulk ks'), False)
        _ ->
            return (Error "ERR unknown command", False)
    respond reply
    unless stop loop

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