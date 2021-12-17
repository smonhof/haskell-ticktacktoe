module Render
    ( render
    ) where

import GameData
import Data.List
import Data.Foldable
import TurnUtility

render :: GameState -> IO ()
render state = do
    renderLine ["-", "-", "-"]
    renderBoard (field state)
    renderLine ["-", "-", "-"]

renderBoard :: [FieldState] -> IO ()
renderBoard array = mapM_ (renderLine . convertRowToStringArray) (chunksOf 3 array)

renderLine :: [String] -> IO ()
renderLine content = putStrLn ("|" ++ concat content ++ "|")

convertRowToStringArray :: [FieldState] -> [String]
convertRowToStringArray = map renderField

renderField :: FieldState -> String
renderField (Empty _) = "-"
renderField (Filled player) = label player

