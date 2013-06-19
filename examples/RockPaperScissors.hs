{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Applicative
import Control.Concurrent.STM
import Data.Default
import Data.Typeable
import Data.Serialize ( Serialize )
import GHC.Generics ( Generic )
import Language.Sexp

import Ltc.Store
import Ltc.Store.Simple
import Ltc.Network.Interface
import Ltc.Network.Interface.UDP
import qualified Ltc.Network.NodeServer as Node

data RPS = Rock | Paper | Scissors
         deriving ( Eq, Show, Read, Generic, Typeable, Ord )

instance Storable RPS

instance Sexpable RPS

instance Serialize RPS

instance Diffable RPS where
    data Diff RPS = ReplaceDiff RPS RPS
                    deriving ( Eq, Generic, Show )

    diffFromTo = ReplaceDiff

    applyDiff x1 (ReplaceDiff x2 y) =
        if x1 == x2 then y else error "cannot apply diff to RPS"

    reverseDiff (ReplaceDiff x y) = ReplaceDiff y x

instance Default RPS where
    def = Rock

instance Sexpable (Diff RPS)

instance Serialize (Diff RPS)

hostRpsRound :: (Store s) => s -> IO ()
hostRpsRound _store = do
    return ()

playRpsRound :: NetworkLocation UdpInterface -> IO ()
playRpsRound opponent = do
    -- Open the local store.
    store <- open (SimpleParameters
                       { location = "rps-store"
                       , nodeName = "rps-client"
                       })

    -- Connect to the host node.
    node <- Node.serve store "rps-client"
    Node.addNeighbour node "rps-host" opponent

    -- Make a choice of rock, paper, or scissors.
    choice <- read <$> getLine
    _ <- set store "player:2:choice" (choice :: RPS)

    -- Block until other player makes their choice and get it.
    waitForSetKey store "player:1:choice"
    Just (otherChoice, _) <- getLatest store "player:1:choice"

    -- Decide round.
    case (choice, otherChoice) of
        (Rock, Paper)     -> putStrLn "You lose."
        (Rock, Scissors)  -> putStrLn "You win."
        (Paper, Rock)     -> putStrLn "You win."
        (Paper, Scissors) -> putStrLn "You lose."
        (Scissors, Rock)  -> putStrLn "You lose."
        (Scissors, Paper) -> putStrLn "You win."
        _                 -> putStrLn "You tie."

-- FIXME waitForSetKey is so general, it should be part of LTc.
waitForSetKey :: (Store s) => s -> Key -> IO ()
waitForSetKey store key = do
    evChan <- newTChanIO
    addEventChannel store evChan
    loop evChan
  where
    loop chan = do
        ev <- atomically $ readTChan chan
        case ev of
            MSetEvent sets | key `elem` map setKey sets ->
                return ()
            _ ->
                loop chan

main :: IO ()
main = do
    putStrLn "Ok"
