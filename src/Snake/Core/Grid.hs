{-# LANGUAGE LambdaCase #-}

module Snake.Core.Grid (
  gridHeight,
  gridWidth,
  Coordinate,
  Direction (..),
  nextPosition,
) where

gridHeight :: Int
gridHeight = 40

gridWidth :: Int
gridWidth = 40

-- | Coordinates on the plane, with (0, 0) being the top-left corner
type Coordinate = (Int, Int)

data Direction = LEFT | UP | RIGHT | DOWN
  deriving (Show)

nextPosition :: Direction -> Coordinate -> Coordinate
nextPosition = \case
  LEFT -> move (-1) 0
  UP -> move 0 (-1)
  RIGHT -> move 1 0
  DOWN -> move 0 1
  where
    move dx dy (x, y) =
      ( clamp 0 (gridWidth - 1) (x + dx)
      , clamp 0 (gridHeight - 1) (y + dy)
      )
    clamp lo hi = min hi . max lo
