module Main where

import Game.Game

import Data.List
import Control.Monad
import Control.Concurrent

import Debug.Trace

main = do
  kittenstates <- newMVar []
  initialize [
    GameDescriptor {
      playGame = playExplodingKittens kittenstates,
      descName = "ExplodingKittens",
      shardNames = ["Meow","Kittens","Explode","Unicorn"],
      onMessage = kittenMessage kittenstates}
    ]


data Card = DefuseCard
          | NopeCard
          | ExplodingKittenCard
          | AtackCard
          | SkipCard
          | FavorCard
          | ShuffleCard
          | SeeFutureCard
          | ComboCard Int deriving (Eq, Show)

data Player = Player {plaCli :: Client, hand :: [Card], name :: String, comm :: MVar String} deriving Eq
instance Show Player where
  show = name
data KittenState = KittenState {playerList :: [Player], deck :: [Card]} deriving Eq

unshuffledDeck :: [Card]
unshuffledDeck = 
  replicate 5 NopeCard ++
  replicate 4 AtackCard ++
  replicate 4 SkipCard ++
  replicate 4 FavorCard ++
  replicate 4 ShuffleCard ++
  replicate 5 SeeFutureCard ++
  (concat . replicate 4 $ map ComboCard [1..5])

additionalCards :: KittenState -> [Card]
additionalCards ks = 
  replicate (6 - (length . playerList $ ks)) DefuseCard ++ 
  replicate (subtract 1 . length . playerList $ ks) ExplodingKittenCard

shuffleDeck :: KittenState -> IO KittenState
shuffleDeck = return

dealCards :: KittenState -> KittenState
dealCards ks = iterate dealPlayer ks !! (length . playerList $ ks)

dealPlayer :: KittenState -> KittenState
dealPlayer ks = ks {
  deck = drop 4 (deck ks), 
  playerList = (last . playerList $ ks) {hand = DefuseCard : take 4 (deck ks)} : init (playerList ks)}

clientToPlayer :: Client -> IO Player
clientToPlayer cli = newEmptyMVar >>= \x -> return Player {plaCli = cli, name = cliName cli, hand = [], comm = x}

prepareDeck :: KittenState -> IO KittenState
prepareDeck ks = do
  ks' <- shuffleDeck ks
  shuffleDeck . (\x -> x {deck = deck x ++ additionalCards ks'}) . dealCards $ ks'
  
playExplodingKittens :: MVar [KittenState] -> [Client] -> IO StopCode
playExplodingKittens ks cls = do
  players <- mapM clientToPlayer cls
  let ourState = KittenState {playerList = players, deck = unshuffledDeck}
  newState <- prepareDeck ourState
  modifyMVar_ ks $ return . (newState:)

  return $ Stop "Meow"

playTurn :: KittenState -> Player -> IO KittenState
playTurn ks pla = do
  askClient "Action?" (plaCli pla)
  resp <- takeMVar (comm pla)
  case () of
   _| "Draw" == resp -> return ()
    | "Play" `isPrefixOf` resp -> do
      let playedCard = drop 4 resp
      return ()
      
  return ks
  
drawForPlayer :: KittenState -> Player -> KittenState
drawForPlayer ks pla = ks {
  deck = tail . deck $ ks,
  playerList = pla {hand = head (deck ks) : hand pla} : delete pla (playerList ks)}

kittenMessage :: MVar [KittenState] -> Client -> String -> IO ()
kittenMessage ks c s = do 
  kss <- readMVar ks
  forM_ kss $ mapM_ (\x -> when (plaCli x == c) $ putMVar (comm x) s). playerList
  putStrLn $ "[Exploding Kittens] Got message: \"" ++ s ++ "\" from client " ++ cliName c

