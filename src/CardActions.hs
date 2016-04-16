module CardActions where

import KittenData
import KittenUtil

import Control.Applicative

type CardAction = Player -> KittenState -> IO (Bool, KittenState)

cardAction :: Card -> CardAction
cardAction pc = case pc of
  AttackCard    -> playAttackCard 
  FavorCard     -> playFavorCard
  SkipCard      -> playSkipCard
  ShuffleCard   -> playShuffleCard
  SeeFutureCard -> playSeeFutureCard

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
playSkipCard _ = return . (True ,) 

playShuffleCard :: CardAction
-- add (False ,) to the result of shuffleDeck on the arg 
playShuffleCard _ = ((,) False `fmap`) . shuffleDeck 

playSeeFutureCard :: CardAction
playSeeFutureCard pla ks = tellPlayer (show . take 3 . deck $ ks) pla >> return (False,ks)


drawCard :: Player -> KittenState -> IO KittenState
drawCard pla ks = do
  
