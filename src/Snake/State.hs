{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Snake.State (
  GameState (..),
  mkInitialState,
  setNewTarget,
) where

import Data.Set qualified as Set
import System.Random (randomRIO)

import Snake.Grid (
  Coordinate,
  Movement (..),
  gridHeight,
  gridWidth,
 )

data GameState = GameState
  { millisPerFrame :: Int     -- ^ Number of milliseconds per frame
  , snakeBody :: [Coordinate] -- ^ Coordinates of the snake's body
  , snakeMovement :: Movement -- ^ Current movement of the snake
  , targetPos :: Coordinate   -- ^ Position of the target
  } deriving (Show)

mkInitialState :: IO GameState
mkInitialState = do
  let snakeBody = [(0, 0)]
  targetPos <- generateCoordinateNotIn snakeBody
  return
    GameState
      { millisPerFrame = 1000
      , snakeBody
      , snakeMovement = NoMove
      , targetPos
      }

-- | Randomly generate a new target.
setNewTarget :: GameState -> IO GameState
setNewTarget state = do
  targetPos <- generateCoordinateNotIn $ snakeBody state
  return state{targetPos}

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
