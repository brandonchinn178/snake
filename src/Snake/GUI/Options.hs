module Snake.GUI.Options (
  GameOptions (..),
  GameMode (..),
  BotStrategy,

  -- * Re-exports
  Grid (..),
) where

import Snake.Core.Grid (Grid (..))
import Snake.Strategy (BotStrategy)

data GameOptions = GameOptions
  { gameMode :: GameMode
  , initialMillisPerFrame :: Int
  , gameGrid :: Grid
  , maxBoardHeight :: Int
  , maxBoardWidth :: Int
  }

data GameMode
  = Interactive
  | RunBot BotStrategy
