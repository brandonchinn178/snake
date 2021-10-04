module Snake.GUI.Options (
  GameOptions (..),
  Grid (..),
) where

import Snake.Core.Grid (Grid (..))

data GameOptions = GameOptions
  { initialMillisPerFrame :: Int
  , gameGrid :: Grid
  , maxBoardHeight :: Int
  , maxBoardWidth :: Int
  }
