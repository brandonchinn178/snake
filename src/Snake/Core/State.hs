{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Snake.Core.State (
  GameState (..),
  snakeBody,
  mkInitialState,
  getNextState,
) where

import Data.Set qualified as Set
import System.Random (randomRIO)

import Snake.Core.Grid (
  Coordinate,
  Direction (..),
  flipDirection,
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

snakeBody :: GameState -> [Coordinate]
snakeBody GameState{snakeHead, snakeTail} = getSnakeBody snakeHead snakeTail

getSnakeBody :: Coordinate -> [Direction] -> [Coordinate]
getSnakeBody = scanl (flip nextPosition)

mkInitialState :: IO GameState
mkInitialState = do
  let snakeHead = (0, 0)
      snakeTail = []
  target <- generateCoordinateNotIn $ getSnakeBody snakeHead snakeTail
  return
    GameState
      { millisPerFrame = 500
      , snakeHead
      , snakeTail
      , snakeMovement = Nothing
      , target
      }

-- | Get the next state.
getNextState :: GameState -> IO GameState
getNextState state@GameState{..} = do
  let snakeHead' = maybe id nextPosition snakeMovement $ snakeHead
      gotTarget = snakeHead' == target

      snakeTail' =
        case flipDirection <$> snakeMovement of
          Nothing -> snakeTail
          Just nextTailPart
            | gotTarget -> nextTailPart : snakeTail
            | null snakeTail -> []
            | otherwise -> nextTailPart : init snakeTail

  target' <-
    if gotTarget
      then generateCoordinateNotIn $ getSnakeBody snakeHead' snakeTail'
      else pure target

  let millisPerFrame' =
        if gotTarget
          then max 10 $ millisPerFrame - 25
          else millisPerFrame

  return state
    { millisPerFrame = millisPerFrame'
    , snakeHead = snakeHead'
    , snakeTail = snakeTail'
    , target = target'
    }

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
