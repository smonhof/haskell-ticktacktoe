module AiController where

import GameData
import TurnUtility
import Data.Maybe

calculateMove :: GameState -> Maybe Int
calculateMove state = firstVal (field state) [getWinningMove (currentPlayer state), getWinningMove(otherPlayer state), getFirstFree]

getFirstFree :: [FieldState] -> Maybe Int
getFirstFree [] = Nothing
getFirstFree ((Filled _):xs) = getFirstFree xs
getFirstFree (Empty x:_) = Just x

getWinningMove :: Player -> [FieldState] -> Maybe Int
getWinningMove _ [] = Nothing
getWinningMove p board = first (toMaybe(getCandidate p)) (getTestLines board)

firstVal :: a -> [(a -> Maybe b)] -> Maybe b
firstVal val (f:xs)
  | isJust (f val) = f val
  | otherwise = firstVal val xs

first :: (a -> Maybe b) -> [a] -> Maybe b
first _ [] = Nothing
first f (x:xs)
  | isJust (f x) = f x
  | otherwise = first f xs

toMaybe :: (a -> Search b) -> (a -> Maybe b)
toMaybe f = conv . f  where
  conv :: Search b -> Maybe b
  conv Fail = Nothing
  conv Valid = Nothing
  conv (Success s) = Just s

getCandidate :: Player -> [FieldState] -> Search Int
getCandidate _ [] = Fail
getCandidate p fields | any isNotPlayer fields = Fail where
  isNotPlayer :: FieldState -> Bool
  isNotPlayer (Filled i) | i /= p = True
  isNotPlayer _ = False
getCandidate _ fields = foldr getAppropriate Valid (map fieldToInt fields) where
  getAppropriate :: Search Int -> Search Int -> Search Int
  getAppropriate Valid r = r
  getAppropriate l Valid = l
  getAppropriate l r = Fail
  fieldToInt :: FieldState -> Search Int
  fieldToInt (Filled _) = Valid
  fieldToInt (Empty e) = Success e

data Search a = Fail | Valid | Success a

