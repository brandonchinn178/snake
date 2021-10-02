{-# LANGUAGE LambdaCase #-}

module Snake.Grid (
  gridHeight,
  gridWidth,
  Coordinate,
  Movement (..),
  nextPosition,
) where

gridHeight :: Int
gridHeight = 20

gridWidth :: Int
gridWidth = 20

-- | Coordinates on the plane, with (0, 0) being the top-left corner
type Coordinate = (Int, Int)

data Movement = MoveLeft | MoveUp | MoveRight | MoveDown | NoMove
  deriving (Show)

nextPosition :: Movement -> Coordinate -> Coordinate
nextPosition = \case
  MoveLeft -> move (-1) 0
  MoveUp -> move 0 (-1)
  MoveRight -> move 1 0
  MoveDown -> move 0 1
  NoMove -> move 0 0
  where
    move dx dy (x, y) =
      ( clamp 0 (gridWidth - 1) (x + dx)
      , clamp 0 (gridHeight - 1) (y + dy)
      )
    clamp lo hi = min hi . max lo
