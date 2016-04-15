module CardActions where

import KittenData
import KittenUtil

import Control.Applicative

type SignalState = IO (Bool, KittenState)
type CardAction = Player -> KittenState -> SignalState

cardAction :: Card -> CardAction
cardAction pc = case pc of
  "AttackCard"   -> playAttackCard 
  "FavorCard"    -> playFavorCard
  "Skip"         -> playSkipCard
  "Shuffle"      -> playShuffleCard
  "SeeFutureCard"-> playSeeFutureCard

playAttackCard :: CardAction
playAttackCard pla ks = do
  target <- getPlayer ks <$> askPlayerUntil (isPlayer ks) "Attack Target" pla 
  return (True, ks {nextPlayers = [target, target]})

playFavorCard :: CardAction
playFavorCard pla ks = do
  target <- getPlayer ks <$> askPlayerUntil (isPlayer ks) "Favor Target" pla 
  return (False, 
    changePlayer pla (pla {hand = head (hand target) : hand pla}) $ 
    changePlayer target (target {hand = tail (hand target)}) ks)

playSkipCard :: CardAction
playSkipCard pla ks = return (True, ks)

playShuffleCard :: CardAction
