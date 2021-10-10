{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Snake.Core.Grid (
  Grid (..),
  Coordinate,
  Direction (..),
  flipDirection,
  nextPosition,
  isOutOfBounds,
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Grid = Grid
  { gridHeight :: Int
  , gridWidth :: Int
  } deriving (Show, Generic, NFData)

-- | Coordinates on the plane, with (0, 0) being the top-left corner
type Coordinate = (Int, Int)

data Direction = LEFT | UP | RIGHT | DOWN
  deriving (Show, Enum, Bounded, Generic, NFData)

flipDirection :: Direction -> Direction
flipDirection = \case
  LEFT -> RIGHT
  UP -> DOWN
  RIGHT -> LEFT
  DOWN -> UP

nextPosition :: Direction -> Coordinate -> Coordinate
nextPosition = \case
  LEFT -> move (-1) 0
  UP -> move 0 (-1)
  RIGHT -> move 1 0
  DOWN -> move 0 1
  where
    move dx dy (x, y) = (x + dx, y + dy)

isOutOfBounds :: Grid -> Coordinate -> Bool
isOutOfBounds Grid{..} (x, y) =
  or
    [ x < 0
    , x >= gridWidth
    , y < 0
    , y >= gridHeight
    ]
