module Main where

import Game.Game

import KittenData
import KittenUtil
import CardActions

import Data.List
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Arrow

import Debug.Trace

type CommsList = MVar [(MVar String, String)]
main = do
  commMVar <- newMVar []
  initialize [
    GameDescriptor {
      playGame = playExplodingKittens commMVar,
      descName = "ExplodingKittens",
      shardNames = ["Meow","Kittens","Explode","Unicorn"],
      onMessage = kittenMessage commMVar}
    ]
 
playExplodingKittens :: CommsList -> [Client] -> IO StopCode
playExplodingKittens comms cls = do
  players <- mapM clientToPlayer cls
  let ourState = KittenState {nextPlayers = [], playerList = players, deck = unshuffledDeck}
  -- Prepare the game to go
  newState <- prepareDeck ourState
  -- Add it to the game list
  modifyMVar_ comms $ return . (map (comm &&& name) players ++)
  ks' <- gameLoop newState
  return $ Stop "Meow"

gameLoop :: KittenState -> IO KittenState
gameLoop ks = 
  if length (playerList ks) > 1
    then do
      putStrLn "GameLoop"
      let override = null (nextPlayers ks)
      let currPlayer = head $ (if override then playerList else nextPlayers) ks
      let ks' = ks {
        nextPlayers = if override then [] else tail (nextPlayers ks),
        playerList = if override then tail (playerList ks) ++ [head (playerList ks)] else playerList ks}
      playTurn currPlayer ks' >>= gameLoop
    else return ks
  
-- Play a single player's turn
playTurn :: PlayerAction 
playTurn pla ks = do
  resp <- askPlayerUntil (`elem` possibleActions) "Action?" pla
  case () of
   _| "Draw" == resp -> drawCard pla ks 
    | "Play" `isPrefixOf` resp -> do
      let playedCard = drop 4 resp 
      (endTurn,ks') <- cardAction playedCard pla ks
      if endTurn
        then return ks'
        else playTurn pla ks'

kittenMessage :: CommsList -> Client -> String -> IO ()
kittenMessage commMVar c s = do 
  comms <- readMVar commMVar
  mapM_ (\x -> when (snd x == cliName c) $ putMVar (fst x) s) comms
  putStrLn $ "[Exploding Kittens] Got message: \"" ++ s ++ "\" from client " ++ cliName c

