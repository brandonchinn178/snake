{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Snake.Core.State (
  GameState (..),
  snakeBody,
  mkInitialState,
  getNextState,
  GameStatus (..),
  isRunning,
) where

import Data.Set qualified as Set
import System.Random (randomRIO)

import Snake.Core.Grid (
  Coordinate,
  Direction (..),
  flipDirection,
  gridHeight,
  gridWidth,
  isOutOfBounds,
  nextPosition,
 )

data GameState = GameState
  { gameStatus :: GameStatus
  , snakeHead :: Coordinate  -- ^ Coordinate of the snake's head
  , snakeTail :: [Direction] -- ^ Directions for each subsequent piece of the snake's body
  , target :: Coordinate     -- ^ Coordinate of the target
  } deriving (Show)

snakeBody :: GameState -> [Coordinate]
snakeBody GameState{snakeHead, snakeTail} = getSnakeBody snakeHead snakeTail

getSnakeBody :: Coordinate -> [Direction] -> [Coordinate]
getSnakeBody = scanl (flip nextPosition)

mkInitialState :: IO GameState
mkInitialState = do
  let snakeHead = (gridWidth `div` 2, gridHeight `div` 2)
      snakeTail = []
  target <- generateCoordinateNotIn $ getSnakeBody snakeHead snakeTail
  return
    GameState
      { gameStatus = SnakeWaiting
      , snakeHead
      , snakeTail
      , target
      }

data GameStatus
  = SnakeWaiting
  | SnakeHissingTowards Direction
  | SnakeRanIntoWall
  | SnakeAteItself
  deriving (Show)

isRunning :: GameStatus -> Bool
isRunning = \case
  SnakeWaiting -> True
  SnakeHissingTowards{} -> True
  SnakeRanIntoWall -> False
  SnakeAteItself -> False

-- | Get the next state.
getNextState :: GameState -> IO GameState
getNextState state@GameState{..} = do
  let snakeHead' =
        case gameStatus of
          SnakeHissingTowards dir -> nextPosition dir snakeHead
          _ -> snakeHead
      gotTarget = snakeHead' == target
      snakeTail' =
        case gameStatus of
          SnakeHissingTowards dir
            | gotTarget -> flipDirection dir : snakeTail
            | null snakeTail -> []
            | otherwise -> flipDirection dir : init snakeTail
          _ -> snakeTail

      snakeBody' = getSnakeBody snakeHead' snakeTail'
      gameStatus'
        | isOutOfBounds snakeHead' = SnakeRanIntoWall
        | snakeHead' `elem` tail snakeBody' = SnakeAteItself
        | otherwise = gameStatus

  target' <-
    if gotTarget
      then generateCoordinateNotIn snakeBody'
      else pure target

  return $
    if isRunning gameStatus'
      then
        state
          { gameStatus = gameStatus'
          , snakeHead = snakeHead'
          , snakeTail = snakeTail'
          , target = target'
          }
      else
        state
          { gameStatus = gameStatus'
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
