module Main where

import Game.Game

import KittenData
import KittenUtil
import CardActions

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
 
playExplodingKittens :: MVar [KittenState] -> [Client] -> IO StopCode
playExplodingKittens ks cls = do
  players <- mapM clientToPlayer cls
  let ourState = KittenState {playerList = players, deck = unshuffledDeck}
  -- Prepare the game to go
  newState <- prepareDeck ourState
  -- Add it to the game list
  modifyMVar_ ks $ return . (newState:)

  return $ Stop "Meow"

-- Play a single player's turn
playTurn :: KittenState -> Player -> IO KittenState
playTurn ks pla = do
  askClient "Action?" (plaCli pla)
  resp <- takeMVar (comm pla)
  case () of
   _| "Draw" == resp -> return ()
    | "Play" `isPrefixOf` resp -> do
      let playedCard = drop 4 resp
      (endTurn,ks') <- cardAction playedCard pla ks
      if endTurn
        then return ks'
        else playTurn ks' pla
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

