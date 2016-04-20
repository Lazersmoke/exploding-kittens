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


cardAction :: Card -> PlayerActionSignal
cardAction pc pla ks = 
  if pc `elem` hand pla
    then do
      consoleLog $ name pla ++ " played a " ++ show pc
      let ks' = changePlayer pla (pla {hand = delete pc (hand pla)}) ks
      let pla' = getPlayer ks' (name pla)
      case pc of
        AttackCard    -> return . (, ks') . Just . head . tail . dropWhile (/=pla') . cycle $ playerList ks'
        FavorCard     -> do
          target <- getPlayer ks' <$> askPlayerUntil (isPlayer ks') "Favor Target" pla'
          targetCard <- getCard <$> askPlayerUntil (flip elem (hand target) . getCard) "Favor Card" target
          return (Just pla', 
            changePlayer pla' (pla' {hand = targetCard : hand pla'}) $ 
            changePlayer target (target {hand = delete targetCard (hand target)}) ks')
        SkipCard      -> return (Nothing, ks') 
        ShuffleCard   -> ((Just pla' ,) `fmap`) . shuffleDeck $ ks'
        SeeFutureCard -> do
          tellPlayer (show . take 3 . deck $ ks') pla'
          return (Just pla', ks')
        (ComboCard x) -> 
          if ComboCard x `elem` hand pla' 
            then do
              target <- getPlayer ks' <$> askPlayerUntil (isPlayer ks') "Favor Target" pla'
              let pla'' = pla' {hand = head (hand target) : delete (ComboCard x) (hand pla')}
              return (Just pla'', changePlayer target (target {hand = tail (hand target)}) . changePlayer pla' pla'' $ ks')
            else return (Just pla, ks)
    else return (Just pla, ks)

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
