module TurnUtility (
  isFieldPlayable
  ) where

import GameData

isFieldPlayable :: [FieldState] -> Int -> Bool
isFieldPlayable _ i | i < 0 = False
isFieldPlayable _ i | i > 9 = False
isFieldPlayable state index = state !! index == Empty