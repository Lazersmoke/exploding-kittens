{-# LANGUAGE TupleSections #-}
module CardActions (
  cardAction,
  drawCard) where

import KittenData
import KittenUtil

import Data.Char (isDigit)
import Data.List
import Control.Applicative


cardAction :: String -> PlayerActionSignal
cardAction pc = case pc of
  "AttackCard"    -> playAttackCard 
  "FavorCard"     -> playFavorCard
  "SkipCard"      -> playSkipCard
  "ShuffleCard"   -> playShuffleCard
  "SeeFutureCard" -> playSeeFutureCard

playAttackCard :: PlayerActionSignal
playAttackCard pla ks = do
  target <- getPlayer ks <$> askPlayerUntil (isPlayer ks) "Attack Target" pla 
  return (True, ks {nextPlayers = [target, target]})

playFavorCard :: PlayerActionSignal
playFavorCard pla ks = do
  target <- getPlayer ks <$> askPlayerUntil (isPlayer ks) "Favor Target" pla 
  return (False, 
    changePlayer pla (pla {hand = head (hand target) : hand pla}) $ 
    changePlayer target (target {hand = tail (hand target)}) ks)

playSkipCard :: PlayerActionSignal
playSkipCard _ = return . (True ,) 

playShuffleCard :: PlayerActionSignal
-- add (False ,) to the result of shuffleDeck on the arg 
playShuffleCard _ = ((False ,) `fmap`) . shuffleDeck 

playSeeFutureCard :: PlayerActionSignal
playSeeFutureCard pla ks = tellPlayer (show . take 3 . deck $ ks) pla >> return (False,ks)


drawCard :: Player -> KittenState -> IO KittenState
drawCard pla ks = do
  let drawn = head . deck $ ks
  let ks' = ks {deck = tail . deck $ ks}
  tellPlayer (("You Drew: " ++) . show $ drawn) pla
  case drawn of
    ExplodingKittenCard -> 
      if DefuseCard `elem` hand pla
        then defuseKitten pla ks'
        else killPlayer pla ks'
    _ -> return (changePlayer pla (pla {hand = drawn : hand pla}) ks') {deck = tail . deck $ ks'}


defuseKitten :: Player -> KittenState -> IO KittenState
defuseKitten pla ks = do
  tellPlayer "You Defused the Kitten" pla
  position <- read <$> askPlayerUntil ((==) "" . dropWhile isDigit) "Return Kitten Location" pla
  let splitDeck = splitAt position (deck ks)
  return (changePlayer pla (pla {hand = delete DefuseCard $ hand pla}) ks) {deck = fst splitDeck ++ [ExplodingKittenCard] ++ snd splitDeck}

killPlayer :: Player -> KittenState -> IO KittenState
killPlayer pla ks = do
  tellPlayer "You Exploded" pla
  return ks {playerList = delete pla (playerList ks)}
