{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Main where

import Control.Applicative
import Data.Default
import Data.Maybe ( catMaybes )
import Data.Serialize ( Serialize )
import Data.String ( fromString )
import Data.Typeable
import GHC.Generics ( Generic )
import Language.Sexp
import Network.BSD ( getHostName )
import qualified Data.Set as S
import System.IO.Unsafe ( unsafePerformIO )

import Ltc.Store
import Ltc.Store.Simple
import qualified Ltc.Network.NodeServer as Node

data Bid = Bid Integer
           deriving ( Generic, Typeable, Eq, Ord )

instance Storable Bid

instance Default Bid where
    def = Bid def

instance Serialize Bid

instance Sexpable Bid

instance Diffable Bid where
    data Diff Bid = DiffBid Integer
                  deriving ( Generic, Show, Eq )

    diffFromTo (Bid n1) (Bid n2) = DiffBid (n2 - n1)
    applyDiff (Bid n) (DiffBid d) = Bid (n + d)
    reverseDiff (DiffBid d) = DiffBid (-d)

    mergeDiffs (DiffBid d1) (DiffBid d2) = DiffBid (max d1 d2)

instance Serialize (Diff Bid)

instance Sexpable (Diff Bid)

type AuctionName = String

myUniqueName :: String
myUniqueName = unsafePerformIO getHostName

placeBid :: (Store s) => s -> AuctionName -> Bid -> IO ()
placeBid store auction bid = do
    _ <- set store (fromString (auction ++ ":bid:" ++ myUniqueName)) bid
    return ()

getBids :: (Store s) => s -> AuctionName -> IO [Bid]
getBids store auction = do
    bidKeys <- keys store (auction ++ ":bid:.*")
    map fst . catMaybes <$> mapM (getLatest store) (S.toList bidKeys)

setupNode :: IO Simple
setupNode = do
    store <- open (def { location = "dBay-store"
                       , nodeName = fromString myUniqueName
                       })
    node <- Node.serve store (fromString myUniqueName)
    Node.handleType node (undefined :: Bid)
    -- FIXME Use real addresses
    Node.addNeighbour node "meh" undefined
    return store

main :: IO ()
main = do
    return ()
