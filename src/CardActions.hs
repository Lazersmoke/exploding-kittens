{-# LANGUAGE TupleSections #-}
module CardActions (
  cardAction,
  drawCard) where

import KittenData
import KittenUtil

import Data.Char (isDigit)
import Data.List
import Control.Applicative
import Debug.Trace


cardAction :: String -> PlayerActionSignal
cardAction pc = case pc of
  "AttackCard"    -> playAttackCard 
  "FavorCard"     -> playFavorCard
  "SkipCard"      -> playSkipCard
  "ShuffleCard"   -> playShuffleCard
  "SeeFutureCard" -> playSeeFutureCard

playAttackCard :: PlayerActionSignal
playAttackCard pla ks = do
  consoleLog $ name pla ++ " played an attack card"
  return . (, ks) . Just . head . tail . dropWhile (/=pla) . cycle $ playerList ks

playFavorCard :: PlayerActionSignal
playFavorCard pla ks = do
  target <- getPlayer ks <$> askPlayerUntil (isPlayer ks) "Favor Target" pla 
  consoleLog $ name pla ++ " played a favor card against " ++ name target
  return (Just pla, 
    changePlayer pla (pla {hand = head (hand target) : hand pla}) $ 
    changePlayer target (target {hand = tail (hand target)}) ks)

playSkipCard :: PlayerActionSignal
playSkipCard pla ks = do
  consoleLog (name pla ++ " played a skip card") 
  return (Nothing, ks) 

playShuffleCard :: PlayerActionSignal
-- add (Just pla ,) to the result of shuffleDeck on the arg 
playShuffleCard pla ks = do
  consoleLog $ name pla ++ " played a shuffle card"
  ((Just pla ,) `fmap`) . shuffleDeck $ ks

playSeeFutureCard :: PlayerActionSignal
playSeeFutureCard pla ks = do
  consoleLog $ name pla ++ " played a see the future card"
  tellPlayer (show . take 3 . deck $ ks) pla 
  return (Just pla,ks)


drawCard :: Player -> KittenState -> IO KittenState
drawCard pla ks = do
  consoleLog $ name pla ++ " drew a card from the deck"
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
  consoleLog $ name pla ++ " defused the exploding kitten"
  tellPlayer "You Defused the Kitten" pla
  position <- read <$> askPlayerUntil ((&&) <$> (""==) . dropWhile isDigit <*> (""/=) . traceShowId) "Return Kitten Location" pla
  let splitDeck = splitAt position (deck ks)
  return (changePlayer pla (pla {hand = delete DefuseCard $ hand pla}) ks) {deck = fst splitDeck ++ [ExplodingKittenCard] ++ snd splitDeck}

killPlayer :: Player -> KittenState -> IO KittenState
killPlayer pla ks = do
  consoleLog $ name pla ++ " exploded!"
  tellPlayer "You Exploded" pla
  return ks {playerList = delete pla (playerList ks)}
