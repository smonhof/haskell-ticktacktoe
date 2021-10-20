module Render
    ( render
    ) where

import GameData
import Data.List
import Data.Foldable

render :: GameState -> IO ()
render state = do
    renderLine ["-", "-", "-"]
    renderBoard (field state)
    renderLine ["-", "-", "-"]

renderBoard :: [FieldState] -> IO ()
renderBoard array = mapM_ (renderLine . convertRowToStringArray) (chunksOf 3 array)

renderLine :: [String] -> IO()
renderLine content = putStrLn ("|" ++ concat content ++ "|")

convertRowToStringArray :: [FieldState] -> [String]
convertRowToStringArray = map renderField

renderField :: FieldState -> String
renderField Empty = "-"
renderField (Filled player) = label player

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n
  
  build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
  build g = g (:) []