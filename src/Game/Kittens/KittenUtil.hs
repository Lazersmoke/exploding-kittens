module Game.Kittens.KittenUtil where

import Game.Kittens.KittenData
import Game.NetworkedGameEngine
import Data.List
import Data.Maybe
import Control.Concurrent
-- Constant representing the unshuffled deck, except the additional cards
unshuffledDeck :: [Card]
unshuffledDeck = 
  replicate 5 NopeCard ++
  replicate 4 AttackCard ++
  replicate 4 SkipCard ++
  replicate 4 FavorCard ++
  replicate 4 ShuffleCard ++
  replicate 5 SeeFutureCard ++
  (concat . replicate 4 $ map ComboCard [1..5])

tellPlayer :: String -> Player -> IO ()
tellPlayer s p = tellClient s (plaCli p)

-- Extra defuses and kittens to add back to deck
additionalCards :: KittenState -> [Card]
additionalCards ks = 
  replicate (6 - (length . playerList $ ks)) DefuseCard ++ 
  replicate (subtract 1 . length . playerList $ ks) ExplodingKittenCard

-- TODO make deck random
shuffleDeck :: KittenState -> IO KittenState
shuffleDeck = return

-- Deal every player
dealCards :: KittenState -> KittenState
dealCards ks = iterate dealPlayer ks !! (length . playerList $ ks)

-- Move last player to first and deal them 4 cards
dealPlayer :: KittenState -> KittenState
dealPlayer ks = ks {
  deck = drop 4 (deck ks), 
  playerList = (last . playerList $ ks) {hand = DefuseCard : take 4 (deck ks)} : init (playerList ks)}

-- Convert a client to a player
clientToPlayer :: Client -> IO Player
clientToPlayer cli = newEmptyMVar >>= \x -> return Player {plaCli = cli, name = cliName cli, hand = [], comm = x}

-- Shuffle the deck, deal the cards, add back the additional cards, shuffle again
prepareDeck :: KittenState -> IO KittenState
prepareDeck ks = do
  ks' <- shuffleDeck ks
  ks'' <- shuffleDeck . (\x -> x {deck = deck x ++ additionalCards ks'}) . dealCards $ ks'
  mapM_ (tellPlayer =<< ("Your hand is: " ++) . show . hand) (playerList ks'')
  return ks''

askPlayerUntil :: (String -> Bool) -> String -> Player -> IO String
askPlayerUntil pred s p = do
  tellPlayer ("Ask|" ++ s) p
  readed <- takeMVar (comm p)
  -- Drop 5 for the "Resp|" header
  if length readed > 5 && pred (drop 5 readed)
    then return $ drop 5 readed
    else askPlayerUntil pred s p

getCard :: String -> Card
getCard s = case s of
  "AttackCard" -> AttackCard 
  "FavorCard" -> FavorCard
  "SkipCard" -> SkipCard
  "ShuffleCard" -> ShuffleCard
  "SeeFutureCard" -> SeeFutureCard
  "ComboCard1" -> ComboCard 1
  "ComboCard2" -> ComboCard 2
  "ComboCard3" -> ComboCard 3
  "ComboCard4" -> ComboCard 4
  "ComboCard5" -> ComboCard 5

changePlayer :: Player -> Player -> KittenState -> KittenState
changePlayer old new ks = ks {playerList = fst broken ++ [new] ++ drop 1 (snd broken)}
  where
    broken = break (==old) $ playerList ks

getPlayer :: KittenState -> String -> Player
getPlayer ks s = fromJust . find ((==s) . name) $ playerList ks

isPlayer :: KittenState -> String -> Bool
isPlayer ks s = isJust . find ((==s) . name) $ playerList ks
