module Lib
    ( mainLoop
    ) where

import GameData
import Render
import System.IO
import PlayerController

mainLoop :: IO ()
mainLoop = do
  gameContext
  putStrLn "Replay (Y/N)"
  hFlush stdout  -- Force rendering all text
  input <- getLine
  case input of
    "n" -> return()
    "N" -> return()
    _ -> mainLoop

gameContext :: IO ()
gameContext = do
  putStrLn "Starting game"
  gameLoop createGameState
  putStrLn "Game Ended"

gameLoop :: GameState -> IO ()
gameLoop oldState =
  evaluateTurn (doTurn oldState)

doTurn :: GameState -> GameState
doTurn oldState = GameState (executeTurn oldState) (otherPlayer oldState) (currentPlayer oldState)

evaluateTurn :: GameState -> IO ()
evaluateTurn state = do
  render state
  if isGameFinished (field state)
    then return ()
    else gameLoop state

isGameFinished ::[FieldState] -> Bool
isGameFinished state = True

createGameState :: GameState
createGameState = GameState createEmptyField (Player InputController "o") (Player AiController "x")

createEmptyField :: [FieldState]
createEmptyField = map (const Empty) [1..9]

executeTurn :: GameState -> [FieldState]
executeTurn state = field state

