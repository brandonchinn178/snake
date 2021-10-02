{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Snake.Core.State (
  GameState (..),
  snakeBody,
  mkInitialState,
  setNewTarget,
) where

import Data.Set qualified as Set
import System.Random (randomRIO)

import Snake.Core.Grid (
  Coordinate,
  Direction (..),
  gridHeight,
  gridWidth,
  nextPosition,
 )

data GameState = GameState
  { millisPerFrame :: Int            -- ^ Number of milliseconds per frame
  , snakeHead :: Coordinate          -- ^ Coordinate of the snake's head
  , snakeTail :: [Direction]         -- ^ Directions for each subsequent piece of the snake's body
  , snakeMovement :: Maybe Direction -- ^ Current movement of the snake
  , target :: Coordinate             -- ^ Coordinate of the target
  } deriving (Show)

mkInitialState :: IO GameState
mkInitialState = do
  let snakeHead = (0, 0)
      snakeTail = []
  target <- generateCoordinateNotIn $ snakeBody' snakeHead snakeTail
  return
    GameState
      { millisPerFrame = 1000
      , snakeHead
      , snakeTail
      , snakeMovement = Nothing
      , target
      }

snakeBody :: GameState -> [Coordinate]
snakeBody GameState{snakeHead, snakeTail} = snakeBody' snakeHead snakeTail

snakeBody' :: Coordinate -> [Direction] -> [Coordinate]
snakeBody' = scanl (flip nextPosition)

-- | Randomly generate a new target.
setNewTarget :: GameState -> IO GameState
setNewTarget state = do
  target <- generateCoordinateNotIn $ snakeBody state
  return state{target}

-- | Generate a new coordinate that's not one of the given coordinates.
generateCoordinateNotIn :: [Coordinate] -> IO Coordinate
generateCoordinateNotIn coords' = go
  where
    coords = Set.fromList coords'
    go = do
      x <- randomRIO (0, gridWidth - 1)
      y <- randomRIO (0, gridHeight - 1)
      let coord = (x, y)
      if coord `Set.member` coords
        then go
        else return coord
