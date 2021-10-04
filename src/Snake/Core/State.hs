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

import Snake.Core.Grid (
  Coordinate,
  Direction (..),
  Grid (..),
  flipDirection,
  isOutOfBounds,
  nextPosition,
 )
import Snake.Core.Targets (NextTargets, findNextTargetWhere)

data GameState = GameState
  { gameGrid :: Grid
  , gameStatus :: GameStatus
  , snakeHead :: Coordinate
  , snakeTail :: [Direction]
  , target :: Coordinate
  }

snakeBody :: GameState -> [Coordinate]
snakeBody GameState{snakeHead, snakeTail} = getSnakeBody snakeHead snakeTail

getSnakeBody :: Coordinate -> [Direction] -> [Coordinate]
getSnakeBody = scanl (flip nextPosition)

mkInitialState :: Grid -> NextTargets -> (GameState, NextTargets)
mkInitialState grid@Grid{..} nextTargets =
  ( GameState
      { gameGrid = grid
      , gameStatus = SnakeWaiting
      , snakeHead
      , snakeTail = []
      , target
      }
  , nextTargets'
  )
  where
    snakeHead = (gridWidth `div` 2, gridHeight `div` 2)
    (target, nextTargets') = findNextTargetWhere (/= snakeHead) nextTargets

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
getNextState :: GameState -> NextTargets -> (GameState, NextTargets)
getNextState state@GameState{..} nextTargets = (state', nextTargets')
  where
    state' =
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

    snakeHead' =
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
      | isOutOfBounds gameGrid snakeHead' = SnakeRanIntoWall
      | snakeHead' `elem` tail snakeBody' = SnakeAteItself
      | otherwise = gameStatus

    (target', nextTargets') =
      if gotTarget
        then findNextTargetWhere (`notElem` snakeBody') nextTargets
        else (target, nextTargets)
