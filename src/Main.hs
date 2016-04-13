module Main where

import Game.Game

import Control.Concurrent

main = do
  kittenstates <- newMVar []
  initialize [
    GameDescriptor {
      playGame = playExplodingKittens kittenstates,
      descName = "ExplodingKittens",
      shardNames = ["Meow","Kittens","Explode","Unicorn"],
      onMessage = kittenMessage kittenstates}
    ]


data Player = Player {plaCli :: Client, name :: String}
data KittenState = KittenState {playerList :: [Player], randomThing :: Int}

clientToPlayer :: Client -> Player
clientToPlayer cli = Player {plaCli = cli, name = cliName cli}

playExplodingKittens :: MVar [KittenState] -> [Client] -> IO StopCode
playExplodingKittens ks cls = do
  let ourState = KittenState {playerList = map clientToPlayer cls, randomThing = 5}
  modifyMVar_ ks $ return . (ourState:)
  do
    putStrLn "Some Stuff happens and ya"
    threadDelay 5000000
  return $ Stop "Meow"

kittenMessage :: MVar [KittenState] -> Client -> String -> IO ()
kittenMessage ks c s = putStrLn $ "[Exploding Kittens] Got message: \"" ++ s ++ "\" from client " ++ cliName c

