module Main where

import Game.Game

import KittenData
import KittenUtil
import CardActions

import Data.List
import Control.Monad
import Control.Applicative
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
 
playExplodingKittens :: MVar [KittenState] -> [Client] -> IO StopCode
playExplodingKittens ks cls = do
  players <- mapM clientToPlayer cls
  let ourState = KittenState {nextPlayers = [], playerList = players, deck = unshuffledDeck}
  -- Prepare the game to go
  newState <- prepareDeck ourState
  -- Add it to the game list
  modifyMVar_ ks $ return . (newState:)
  ks' <- gameLoop newState
  return $ Stop "Meow"

gameLoop :: KittenState -> IO KittenState
gameLoop ks = 
  if length (playerList ks) > 1
    then do
      let ks' = ks {
        nextPlayers = if null (nextPlayers ks) then [] else tail (nextPlayers ks),
        playerList = init (playerList ks) ++ [head (playerList ks)]}
      let currPlayer = head $ (if null (nextPlayers ks) then playerList else nextPlayers) ks
      playTurn currPlayer ks' >>= gameLoop
    else return ks
  
-- Play a single player's turn
playTurn :: PlayerAction 
playTurn pla ks = do
  resp <- askPlayerUntil ((||) <$> (=="Draw") . traceShowId <*> isPrefixOf "Play") "Action?" pla
  case () of
   _| "Draw" == resp -> drawCard pla ks 
    | "Play" `isPrefixOf` resp -> do
      let playedCard = read . drop 4 $ resp
      (endTurn,ks') <- cardAction playedCard pla ks
      if endTurn
        then return ks'
        else playTurn pla ks'

drawForPlayer :: KittenState -> Player -> KittenState
drawForPlayer ks pla = ks {
  deck = tail . deck $ ks,
  playerList = pla {hand = head (deck ks) : hand pla} : delete pla (playerList ks)}

kittenMessage :: MVar [KittenState] -> Client -> String -> IO ()
kittenMessage ks c s = do 
  kss <- readMVar ks
  forM_ kss $ mapM_ (\x -> when (plaCli x == c) $ putMVar (comm x) s). playerList
  putStrLn $ "[Exploding Kittens] Got message: \"" ++ s ++ "\" from client " ++ cliName c

