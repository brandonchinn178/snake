module Snake.GUI.Options (
  GameOptions (..),
  Grid (..),
) where

import Snake.Core.Grid (Grid (..))

data GameOptions = GameOptions
  { gameGrid :: Grid
  }
