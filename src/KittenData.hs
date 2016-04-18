module KittenData where

import Game.Game
import Data.List
import Data.Maybe
import Control.Concurrent

data Card = DefuseCard
          | NopeCard
          | ExplodingKittenCard
          | AttackCard
          | SkipCard
          | FavorCard
          | ShuffleCard
          | SeeFutureCard
          | ComboCard Int deriving (Eq, Show, Read)

possibleActions = [
  "Draw",
  "PlayNopeCard",
  "PlayAttackCard",
  "PlaySkipCard",
  "PlayFavorCard",
  "PlayShuffleCard",
  "PlaySeeFutureCard"
  ]

data Player = Player {
  plaCli :: Client,
  hand :: [Card],
  name :: String,
  comm :: MVar String} deriving Eq

instance Show Player where
  show p = "Player " ++ name p ++ ": " ++ show (hand p)

type PlayerAction = Player -> KittenState -> IO KittenState
type PlayerActionSignal = Player -> KittenState -> IO (Bool,KittenState)

data KittenState = KittenState {
  playerList :: [Player], 
  deck :: [Card],
  nextPlayers :: [Player]} deriving (Eq,Show)


