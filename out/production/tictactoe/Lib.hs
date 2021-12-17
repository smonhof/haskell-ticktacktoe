{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lib
    ( mainLoop
    ) where

import GameData
import Render
import System.IO
import PlayerController
import TurnUtility
import Data.Maybe

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
  checkGameFinished state (isGameFinished (field state))

checkGameFinished :: GameState -> Maybe Player -> IO ()
checkGameFinished state Nothing = gameLoop state
checkGameFinished _ (Just p) = do
  putStrLn ("Player " ++ label p ++ " has won!")
  return ()

isGameFinished ::[FieldState] -> Maybe Player
isGameFinished state = firstResult testLine (getTestLines state) where
  firstResult :: ([FieldState] -> Maybe Player) -> [[FieldState]] -> Maybe Player
  firstResult _ [] = Nothing
  firstResult f (x:xs)
    | isJust (f x) = f x
    | otherwise = firstResult f xs
  testLine :: [FieldState] -> Maybe Player
  testLine [] = Nothing
  testLine ((Empty _) : _) = Nothing
  testLine ((Filled x):xs) = iterate x xs where
    iterate :: Player -> [FieldState] -> Maybe Player
    iterate p [] = Just p
    iterate _ ((Empty _) : _) = Nothing
    iterate p ((Filled p2) : xs)
      | p /= p2 = Nothing
      | otherwise = iterate p xs

createGameState :: GameState
createGameState = GameState createEmptyField (Player InputController "o") (Player AiController "x")

createEmptyField :: [FieldState]
createEmptyField = map Empty [1..9]

executeTurn :: GameState -> [FieldState]
executeTurn state = field state

