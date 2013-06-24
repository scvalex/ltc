{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Main where

import Control.Applicative
import Control.Concurrent ( threadDelay )
import Control.Monad ( forever )
import Data.Default
import Data.Maybe ( catMaybes )
import Data.Serialize ( Serialize )
import Data.String ( fromString )
import Data.Typeable
import GHC.Generics ( Generic )
import Language.Sexp
import Network.BSD ( getHostName )
import qualified Data.Set as S
import System.Environment ( getArgs )
import System.IO.Unsafe ( unsafePerformIO )
import System.Random ( randomRIO )

import Ltc.Network.Interface.UDP
import Ltc.Store
import Ltc.Store.Simple
import qualified Ltc.Network.NodeServer as Node
import qualified Ltc.Network.StatusServer as S

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

setupLtc :: Node.Hostname -> IO Simple
setupLtc otherHost = do
    store <- open (def { location = "dBay-store"
                       , nodeName = fromString myUniqueName
                       })
    node <- Node.serve store (fromString myUniqueName)
    Node.handleType node (undefined :: Bid)
    Node.addNeighbour node
                      (fromString otherHost)
                      (UdpLocation { host = otherHost
                                   , port = Node.nodePort })
    return store

pennyBidder :: (Store s) => s -> AuctionName -> IO ()
pennyBidder store auction = forever $ do
    d <- randomRIO (1000000, 2000000)
    threadDelay d
    bids <- getBids store auction
    let Bid bidAmount = maximum bids
    placeBid store auction (Bid (bidAmount + 3))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["bidder", otherHost] -> do
            store <- setupLtc otherHost
            pennyBidder store "tophat"
        ["observer", otherHost] -> do
            store <- setupLtc otherHost
            _ <- S.serveWithPort S.statusPort store Nothing
            return ()
        _ -> do
            error "wrong command line"
