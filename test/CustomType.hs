{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Common ( cleanEnvironment, testParameters )
import Control.Applicative ( (<$>) )
import Data.Monoid ( mempty )
import Data.Serialize ( Serialize )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Language.Sexp ( Sexpable )
import Ltc.Store ( Store(..), Diffable(..), Storable )
import Network.BSD ( getHostName )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.VectorClock as VC
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMainWithOpts
       [ testCase "set-get" testSetGet
       ] options
  where
    options = mempty { ropt_test_options = Just (mempty { topt_timeout = Just (Just 20000000) }) }

--------------------------------
-- Unit tests
--------------------------------

data Foo = Foo { bar :: Integer }
           deriving ( Eq, Generic, Ord, Typeable, Show )

instance Storable Foo

instance Serialize Foo

instance Sexpable Foo

instance Diffable Foo where
    data Diff Foo = ReplaceDiff Foo Foo
                    deriving ( Eq, Generic, Show )

    diffFromTo = ReplaceDiff

    applyDiff x1 (ReplaceDiff x2 y) =
        if x1 == x2 then y else error "cannot apply diff to Foo"

    reverseDiff (ReplaceDiff x y) = ReplaceDiff y x

instance Serialize (Diff Foo)

instance Sexpable (Diff Foo)

-- | Write a value of a custom type, read it back, and check that we got back the right
-- value.
testSetGet :: Assertion
testSetGet = cleanEnvironment ["test-store"] $ do
    hostname <- BL.pack <$> getHostName
    store <- open testParameters
    _ <- set store "foo" (Foo 23)
    res <- getLatest store "foo"
    res @?= Just (Foo 23, VC.fromList [(hostname, 1)])
    close store
