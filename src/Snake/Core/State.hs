{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Snake.Core.State (
  GameState (..),
  snakeBody,
  mkInitialState,
  getNextState,
  setMovement,
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
getNextState state@GameState{..} nextTargets =
  case gameStatus of
    SnakeHissingTowards dir ->
      let (snakeHead', snakeTail', gotTarget) = getNextSnakePosition snakeHead snakeTail dir target
          snakeBody' = getSnakeBody snakeHead' snakeTail'

          gameStatus'
            | isOutOfBounds gameGrid snakeHead' = SnakeRanIntoWall
            | snakeHead' `elem` tail snakeBody' = SnakeAteItself
            | otherwise = gameStatus

          (target', nextTargets') =
            if gotTarget
              then findNextTargetWhere (`notElem` snakeBody') nextTargets
              else (target, nextTargets)

          state' =
            if isRunning gameStatus'
              then
                state
                  { snakeHead = snakeHead'
                  , snakeTail = snakeTail'
                  , target = target'
                  }
              else
                -- if the snake is dead in the next state, don't move the snake
                state
       in (state'{gameStatus = gameStatus'}, nextTargets')
    _ -> (state, nextTargets)

-- | Get the next position of the snake.
getNextSnakePosition ::
  -- | The current snake head
  Coordinate ->
  -- | The current snake tail
  [Direction] ->
  -- | The current movement
  Direction ->
  -- | The current target
  Coordinate ->
  -- | The next snake head, snake tail, and whether we consumed the target (and thus the
  -- snake tail grew by one)
  (Coordinate, [Direction], Bool)
getNextSnakePosition snakeHead snakeTail dir target = (snakeHead', snakeTail', gotTarget)
  where
    snakeHead' = nextPosition dir snakeHead
    gotTarget = snakeHead' == target
    snakeTail'
      | gotTarget = flipDirection dir : snakeTail
      | null snakeTail = []
      | otherwise = flipDirection dir : init snakeTail


-- | Set the movement in the GameState if the game is running.
setMovement :: GameState -> Direction -> GameState
setMovement gameState dir =
  if isRunning (gameStatus gameState)
    then gameState{gameStatus = SnakeHissingTowards dir}
    else gameState
