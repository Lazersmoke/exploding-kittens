module Game.Kittens.ExplodingKittens (
  playExplodingKittens,
  kittenMessage
  )
  where

import Game.NetworkedGameEngine

import Game.Kittens.KittenData
import Game.Kittens.KittenUtil
import Game.Kittens.CardActions

import Data.Maybe
import Data.List
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Arrow

type CommsList = MVar [(MVar String, String)]

playExplodingKittens :: CommsList -> [Client] -> IO StopCode
playExplodingKittens comms cls = do
  players <- mapM clientToPlayer cls
  let ourState = KittenState {nextPlayers = [], playerList = players, deck = unshuffledDeck}
  -- Prepare the game to go
  newState <- prepareDeck ourState
  -- Add it to the game list
  modifyMVar_ comms $ return . (map (comm &&& name) players ++)
  ks' <- gameLoop newState
  return "Meow"

gameLoop :: KittenState -> IO KittenState
gameLoop ks = do
  print $ deck ks
  if length (playerList ks) > 1
    then do
      -- Are we using a special player sequence?
      let currPlayer = head (playerList ks)
      let ks' = ks {
        playerList = tail (playerList ks) ++ [head (playerList ks)]
        }
      tellPlayer "Info|Your turn has begun" currPlayer
      playTurn currPlayer ks' >>= gameLoop
    else return ks
  
-- Play a single player's turn
playTurn :: PlayerAction 
playTurn pla ks = do
  consoleLog $ "Playing turn for " ++ name pla
  resp <- askPlayerUntil (`elem` possibleActions) "Action?" pla
  case () of
   _| "Draw" == resp -> endTurn >> drawCard pla ks
    | "Play" `isPrefixOf` resp -> do
      let playedCard = fromJust . getCard $ drop 4 resp 
      (nextPlayer,ks') <- cardAction playedCard pla ks
      -- If next player is a Just, play a turn for them. otherwise end our turn
      maybe (endTurn >> return ks') (`playTurn` ks') nextPlayer 
  where
    endTurn = tellPlayer "Info|Your turn has ended" pla

kittenMessage :: CommsList -> Client -> String -> IO ()
kittenMessage commMVar c s = do 
  comms <- readMVar commMVar
  mapM_ (\x -> when (snd x == cliName c) $ putMVar (fst x) s) comms
  -- consoleLog $ "Got message: \"" ++ s ++ "\" from client " ++ cliName c

