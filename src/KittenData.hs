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
          | ComboCard Int deriving (Eq, Show)

data Player = Player {
  plaCli :: Client,
  hand :: [Card],
  name :: String,
  comm :: MVar String} deriving Eq

instance Show Player where
  show = name

data KittenState = KittenState {
  playerList :: [Player], 
  deck :: [Card],
  nextPlayers :: [Player]} deriving Eq


