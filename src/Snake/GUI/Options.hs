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
  , initialFPS :: Int -- ^ The starting frame per second rate
  , fpsMultiplier :: Double -- ^ How much to multiply the FPS per level
  , targetsPerLevel :: Int -- ^ How many targets to consume before incrementing the level
  , gameGrid :: Grid
  , maxBoardHeight :: Int
  , maxBoardWidth :: Int
  }

data GameMode
  = Interactive
  | RunBot BotStrategy
