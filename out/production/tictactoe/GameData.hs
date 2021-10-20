module GameData (
    FieldState (..),
    Player (..),
    GameState (..),
    PlayerController (..)
    ) where


data FieldState = Empty | Filled Player
    deriving (Show, Eq)

data GameState =  GameState {
  field :: [FieldState],
  currentPlayer :: Player,
  otherPlayer :: Player
  }

data Player = Player {
  controller :: PlayerController,
  label :: String
  }
  deriving (Show, Eq)

data PlayerController = InputController | AiController
  deriving (Show, Eq, Enum)