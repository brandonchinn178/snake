{-# LANGUAGE LambdaCase #-}

module Snake.Core.Grid (
  gridHeight,
  gridWidth,
  Coordinate,
  Direction (..),
  flipDirection,
  nextPosition,
  isOutOfBounds,
) where

gridHeight :: Int
gridHeight = 40

gridWidth :: Int
gridWidth = 40

-- | Coordinates on the plane, with (0, 0) being the top-left corner
type Coordinate = (Int, Int)

data Direction = LEFT | UP | RIGHT | DOWN
  deriving (Show)

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

isOutOfBounds :: Coordinate -> Bool
isOutOfBounds (x, y) =
  or
    [ x < 0
    , x >= gridWidth
    , y < 0
    , y >= gridHeight
    ]
