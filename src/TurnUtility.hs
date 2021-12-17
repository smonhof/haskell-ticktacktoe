module TurnUtility (
  isFieldEmptyOnBoard,
  replaceEmptyIndex,
  getTestLines,
  chunksOf,
  columnsOf,
  diagonalsOf
  ) where

import GameData

getFieldForIndex :: [FieldState] -> Int -> Maybe FieldState
getFieldForIndex _ i | i < 1 = Nothing
getFieldForIndex board i | i > length board = Nothing
getFieldForIndex state index = Just (state !! (index - 1))

isFieldEmptyOnBoard :: [FieldState] -> Int -> Bool
isFieldEmptyOnBoard board index = isFieldEmpty (getFieldForIndex board index)

isFieldEmpty :: Maybe FieldState -> Bool
isFieldEmpty Nothing = False
isFieldEmpty (Just (Filled _)) = False
isFieldEmpty (Just (Empty _)) = True

replaceEmptyIndex :: [FieldState] -> Int -> Player -> [FieldState]
replaceEmptyIndex board index player = map (replace index player) board  where
  replace :: Int -> Player -> FieldState -> FieldState
  replace _ _ (Filled f) = Filled f
  replace i _ (Empty e) | e /= i = Empty e
  replace i p _ = Filled p

getTestLines :: [FieldState] -> [[FieldState]]
getTestLines board = chunksOf 3 board ++ columnsOf 3 board ++ diagonalsOf board

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

  build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
  build g = g (:) []

columnsOf :: Int -> [a] -> [[a]]
columnsOf 0 _ = []
columnsOf _ [] = []
columnsOf count source = iterate 0 source (replicate count []) where
  iterate :: Int -> [a] -> [[a]] -> [[a]]
  iterate counter (x:xs) ac = iterate (getCounter counter) xs (insertInto ac counter x) where
    getCounter :: Int -> Int
    getCounter i
      | count < i = i + 1
      | otherwise = 0
    insertInto :: [[a]] -> Int -> a -> [[a]]
    insertInto xs i val = let(ys, zs) = splitAt i xs in ys ++ [head zs ++ [val]] ++ tail zs

diagonalsOf :: [a] -> [[a]]
diagonalsOf [] = []
diagonalsOf board = [[board !! 0, board !! 4, board !! 8], [board !! 2, board !! 4, board !! 6]] -- I have no nerves for this
