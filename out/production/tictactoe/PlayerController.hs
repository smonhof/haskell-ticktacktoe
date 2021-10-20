module PlayerController where

import TurnUtility
import GameData
import System.IO

--doPlayerTurn :: [[FieldState]] -> IO [[FieldState]]
--doPlayerTurn oldState = do
--  putStrLn "Please select a position"
--  hFlush stdout
--  readPlayerTurn oldState



readPlayerTurn :: [FieldState] -> IO Int
readPlayerTurn oldState = do
  input <- getLine
  case tryGetInput oldState input of
    Nothing -> readPlayerTurn oldState
    Just x -> return x

tryGetInput :: [FieldState] -> String -> Maybe Int
tryGetInput field s = do
  candidate <- read s :: Maybe Int
  if isFieldPlayable field candidate
    then Just candidate
    else Nothing