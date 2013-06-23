{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Common ( cleanEnvironment, cleanEnvironmentP, testParameters )
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( forM_, replicateM, when )
import Data.ByteString.Lazy.Char8 ( ByteString )
import Data.Foldable ( foldlM )
import Data.List ( find )
import Data.Monoid ( mempty )
import Data.VectorClock ( causes )
import Ltc.Store ( Store(..)
                 , Key(..), typeOf, Version
                 , NodeNameMismatchError(..), TypeMismatchError(..) )
import Ltc.Store.Simple ( Simple, OpenParameters(..) )
import Ltc.Store.VersionControl ( getDiffPack, insertChangesInto )
import Network.BSD ( getHostName )
import qualified Control.Exception as CE
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.VectorClock as VC
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Monadic as QCM

main :: IO ()
main = defaultMainWithOpts
       [ testCase "open" testOpen
       , testCase "simpleSetGet" testSimpleSetGet
       , testCase "simpleHistory" testSimpleHistory
       , testCase "simpleFieldType" testSimpleFieldType
       , testCase "simpleKeysPattern" testSimpleKeysPattern
       , testProperty "setGetLatest" propSetGetLatest
       , testProperty "keysPresent" propKeysPresent
       , testProperty "ascendingHistory" propAscendingHistory
       , testProperty "fullHistory" propFullHistory
       -- FIXME Re-thing export/import-id test
       -- , testProperty "exportImportId" propExportImportId
       ] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 40000000) }) }

--------------------------------
-- Unit tests
--------------------------------

testOpen :: Assertion
testOpen = cleanEnvironment ["test-store"] $ do
    hostname <- BL.pack <$> getHostName
    store <- open testParameters
    close store
    store' <- open testParameters
    close store'
    opened <- CE.handle (\(exn :: NodeNameMismatchError) ->
                          return (not (requestedName exn == "other"
                                       && storeName exn == hostname))) $ do
        _ <- open (testParameters { nodeName = "other" })
        return True
    when opened $ assertFailure "re-opened store with different node name"

testSimpleSetGet :: Assertion
testSimpleSetGet = cleanEnvironment ["test-store"] $ do
    hostname <- BL.pack <$> getHostName
    store <- open testParameters
    _ <- set store "foo" (vs "bar")
    res1 <- getLatest store "foo"
    res1 @?= Just (vs "bar", VC.fromList [(hostname, 1)])
    res2 <- getLatest store "bar"
    res2 @?= (Nothing :: Maybe (ByteString, Version))
    _ <- set store "bar" (vs "baz")
    res3 <- getLatest store "bar"
    res3 @?= Just (vs "baz", VC.fromList [(hostname, 2)])
    _ <- set store "foo" (vs "boom")
    res4 <- getLatest store "foo"
    res4 @?= Just (vs "boom", VC.fromList [(hostname, 3)])
    ks <- keys store ".*"
    ks @?= S.fromList ["foo", "bar"]
    close store

testSimpleHistory :: Assertion
testSimpleHistory = cleanEnvironment ["test-store"] $ do
    store <- open testParameters
    v1 <- set store "foo" (vs "bar")
    res1 <- getLatest store "foo"
    res1 @?= Just (vs "bar", v1)
    v2 <- set store "foo" (vs "baz")
    res2 <- getLatest store "foo"
    res2 @?= Just (vs "baz", v2)
    res3 <- get store "foo" v1
    res3 @?= Just (vs "bar")
    close store

testSimpleFieldType :: Assertion
testSimpleFieldType = cleanEnvironment ["test-store"] $ do
    store <- open testParameters
    v1 <- set store "foo" (23 :: Integer)
    res1 <- getLatest store "foo"
    res1 @?= Just (23 :: Integer, v1)
    done <- CE.handle (\(exn :: TypeMismatchError) ->
                          return (not (expectedType exn == typeOf (undefined :: Integer))
                                       && foundType exn == typeOf (undefined :: ByteString))) $ do
        _ <- set store "foo" (vs "bar")
        return True
    when done $ assertFailure "set a key with a different type"
    close store

testSimpleKeysPattern :: Assertion
testSimpleKeysPattern = cleanEnvironment ["test-store"] $ do
    store <- open testParameters
    _ <- set store "item1:bid:alex" (23 :: Integer)
    _ <- set store "item1:bid:francesco" (42 :: Integer)
    _ <- set store "item1:bid:alex" (43 :: Integer)
    ks <- keys store "item1:bid:.*"
    ks @?= S.fromList ["item1:bid:alex", "item1:bid:francesco"]

--------------------------------
-- QuickCheck
--------------------------------

instance Arbitrary ByteString where
    arbitrary = sized $ \n -> do
        BL.pack <$> sequence [ choose (' ', '~') | _ <- [1..n] ]

data Command = GetLatest Key
             | Set Key ByteString
             deriving ( Show )

newtype Commands = Commands { unCommands :: [Command] }
                 deriving ( Show )

instance Arbitrary Key where
    arbitrary = Key <$> arbitrary

instance Arbitrary Commands where
    arbitrary = sized $ \n -> do
        let kn = ceiling (sqrt (fromIntegral n :: Double)) :: Int
        ks <- replicateM kn arbitrary
        Commands <$> replicateM n (makeCommand ks)

-- | For any store @store@, and any key @k@, the value of @getLatest store k@ should
-- either be 'Nothing', if @key@ was never set in this store, or @Just (v, vsn)@, where
-- @v@ is parameter to the most recent @set store k v@, and @vsn@ is the returned value of
-- the same command.
propSetGetLatest :: Commands -> Property
propSetGetLatest = propWithCommands (\store cmds -> foldlM (runCmd store) M.empty cmds)
  where
    runCmd store kvs (GetLatest key) = do
        res <- run $ getLatest store key
        QCM.assert (res == M.lookup key kvs)
        return kvs
    runCmd store kvs (Set key value) = do
        vsn <- run $ set store key value
        case M.lookup key kvs of
            Nothing          -> return ()
            Just (_, vsnOld) -> QCM.assert (vsnOld `VC.causes` vsn)
        return (M.insert key (value, vsn) kvs)

-- | For a non-forgetful @store@, /all/ keys @k@ inserted by @set store k v@ should be
-- returned by subsequent @keys store@.
propKeysPresent :: Commands -> Property
propKeysPresent = propWithCommands (\store cmds -> foldlM (runCmd store) S.empty cmds)
  where
    runCmd store s (GetLatest key) = do
        (_ :: Maybe (ByteString, Version)) <- run $ getLatest store key
        return s
    runCmd store s (Set key value) = do
        _ <- run $ set store key value
        let s' = S.insert key s
        ks <- run $ keys store ".*"
        QCM.assert (ks == s')
        return s'

-- | The key versions for any key should always be returned in most-recent-first order.
propAscendingHistory :: Commands -> Property
propAscendingHistory = propWithCommands (\store cmds -> foldlM (runCmd store) () cmds)
  where
    runCmd store () (GetLatest key) = do
        -- We'll actually check the versions here.
        mvsns <- run $ keyVersions store key
        case mvsns of
            Nothing ->
                return ()
            Just vsns ->
                QCM.assert (all (uncurry (flip causes)) (zip vsns (tail vsns)))
        return ()
    runCmd store () (Set key value) = do
        _ <- run $ set store key value
        return ()

-- | For a non-forgetful @store@, /all/ values @v@ inserted by @set store k v$ should
-- still be available to @get store k vsn@, where @vsn@ is the value returned by the
-- corresponding @set@.
propFullHistory :: Commands -> Property
propFullHistory = propWithCommands (\store cmds -> foldlM (runCmd store) (M.empty, []) cmds)
  where
    runCmd store (kvsns, kvs) (GetLatest key) = do
        mvsns <- run $ keyVersions store key
        QCM.assert (mvsns == M.lookup key kvsns)
        case mvsns of
            Nothing ->
                return ()
            Just vsns ->
                -- Theoretically, getting the versions above, and iterating through them
                -- below is a race.  Practically, meh.
                forM_ vsns $ \vsn -> do
                    res <- run $ get store key vsn
                    QCM.assert (res == ((\(_, _, v) -> v)
                                        <$> find (\(k, vsn', _) ->
                                                   k == key && vsn == vsn') kvs))
        return (kvsns, kvs)
    runCmd store (kvsns, kvs) (Set key value) = do
        vsn <- run $ set store key value
        let kvsns' = case M.lookup key kvsns of
                Nothing   -> M.insert key [vsn] kvsns
                Just vsns -> M.insert key (vsn:vsns) kvsns
        return (kvsns', (key, vsn, value):kvs)

-- | Exporting all the changes from a store, importing them into another, exporting those,
-- the two exported sets of changes should be the same.
propExportImportId :: Commands -> Property
propExportImportId cmds = monadicIO $ cleanEnvironmentP ["test-store", "test-store2"] $ do
    store1 <- run $ open testParameters
    forM_ (unCommands cmds) (runCmd store1)
    dp1 <- run $ getDiffPack store1
    run $ close store1
    store2 <- run $ open testParameters { location = "test-store2" }
    conflicts <- run $ insertChangesInto store2 dp1
    QCM.assert (conflicts == [])
    dp2 <- run $ getDiffPack store1
    run $ close store2
    QCM.assert (dp1 == dp2)
  where
    runCmd _ (GetLatest _) = do
        return ()
    runCmd store (Set key value) = do
        _ <- run $ set store key value
        return ()

--------------------------------
-- Helpers
--------------------------------

-- | Given a set of keys, generate an arbitrary command.  @Get@s and @Set@s have equal
-- probability.
makeCommand :: [Key] -> Gen Command
makeCommand ks = do
    n <- choose (1, 2 :: Int)
    let key = elements ks
    case n of
        1 -> GetLatest <$> key
        2 -> Set <$> key <*> arbitrary
        _ -> fail "unknown case in 'Arbitrary Command'"

propWithCommands :: (Simple -> [Command] -> PropertyM IO a) -> Commands -> Property
propWithCommands prop cmds = monadicIO $ do
    cleanEnvironmentP ["test-store"] $ do
        store <- run $ open testParameters
        _ <- prop store (unCommands cmds)
        run $ close store

vs :: String -> ByteString
vs = BL.pack
