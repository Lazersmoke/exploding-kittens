{-# LANGUAGE TupleSections #-}
module Game.Kittens.CardActions (
  cardAction,
  drawCard) where

import Game.Kittens.KittenData
import Game.Kittens.KittenUtil

import Data.Maybe
import Data.Char (isDigit)
import Data.List
import Control.Applicative
import Debug.Trace


cardAction :: Card -> PlayerActionSignal
cardAction pc pla ks = 
  if pc `elem` hand pla 
    then do
      consoleLog $ name pla ++ " played a " ++ show pc
      tellPlayer ("Info|You Played: " ++ show pc) pla
      let ks' = changePlayer pla (pla {hand = delete pc (hand pla)}) ks
      let pla' = getPlayer ks' (name pla)
      case pc of
        AttackCard    -> return . (, ks') . Just . head . tail . dropWhile (/=pla') . cycle $ playerList ks'
        FavorCard     -> do
          target <- getPlayer ks' <$> askPlayerUntil (\x -> maybe (isPlayer ks' x) (/= pla') (getPlayerSafe ks' x)) "Favor Target" pla'
          targetCard <- fromJust . getCard . drop 4 <$> askPlayerUntil (maybe False (`elem` hand target) . getCard . drop 4) "Favor Card" target
          tellPlayer (("Info|You Got: " ++) . show $ targetCard) pla
          tellPlayer (("Info|You Lost: " ++) . show $ targetCard) target
          return (Just pla', 
            changePlayer pla' (pla' {hand = targetCard : hand pla'}) $ 
            changePlayer target (target {hand = delete targetCard (hand target)}) ks')
        SkipCard      -> return (Nothing, ks') 
        NopeCard      -> return (Nothing, ks') 
        ShuffleCard   -> ((Just pla' ,) `fmap`) . shuffleDeck $ ks'
        SeeFutureCard -> do
          tellPlayer (("Info|The next three cards are: " ++) . show . take 3 . deck $ ks') pla'
          return (Just pla', ks')
        (ComboCard x) -> 
          if traceShowId $ ComboCard x `elem` traceShowId (hand pla')
            then do
              tellPlayer (("Info|You Lost: " ++) . show $ pc) pla
              target <- getPlayer ks' <$> askPlayerUntil (\x -> maybe (isPlayer ks' x) (/= pla') (getPlayerSafe ks' x)) "Combo Target" pla'
              let pla'' = pla' {hand = head (hand target) : delete (ComboCard x) (hand pla')}
              tellPlayer (("Info|You Got: " ++) . show . head . hand $ target) pla
              tellPlayer (("Info|You Lost: " ++) . show . head . hand $ target) target
              return (Just pla'', changePlayer target (target {hand = tail (hand target)}) . changePlayer pla' pla'' $ ks')
            else return (Just pla, ks)
    else return (Just pla, ks)

drawCard :: Player -> KittenState -> IO KittenState
drawCard pla ks = do
  consoleLog $ name pla ++ " drew a card from the deck"
  let drawn = head . deck $ ks
  let ks' = ks {deck = tail . deck $ ks}
  tellPlayer (("Info|You Drew: " ++) . show $ drawn) pla
  case drawn of
    ExplodingKittenCard -> 
      if DefuseCard `elem` hand pla
        then defuseKitten pla ks'
        else killPlayer pla ks'
    _ -> return (changePlayer pla (pla {hand = drawn : hand pla}) ks')


defuseKitten :: Player -> KittenState -> IO KittenState
defuseKitten pla ks = do
  consoleLog $ name pla ++ " defused the exploding kitten"
  tellPlayer "Info|You Defused the Kitten" pla
  tellPlayer "Info|You Lost: DefuseCard" pla
  position <- read <$> askPlayerUntil ((&&) <$> (""==) . dropWhile isDigit <*> (""/=) . traceShowId) "Return Kitten Location" pla
  let splitDeck = splitAt position (deck ks)
  return (changePlayer pla (pla {hand = delete DefuseCard $ hand pla}) ks) {deck = fst splitDeck ++ [ExplodingKittenCard] ++ snd splitDeck}

killPlayer :: Player -> KittenState -> IO KittenState
killPlayer pla ks = do
  consoleLog $ name pla ++ " exploded!"
  tellPlayer "Info|You Exploded" pla
  tellPlayer "Stop|You Exploded" pla
  tellPlayer "Info|You Lost: ExplodingKittenCard" pla
  return ks {playerList = delete pla (playerList ks)}
