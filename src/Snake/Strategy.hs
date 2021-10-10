{-# LANGUAGE NamedFieldPuns #-}

module Snake.Strategy (
  BotStrategy,
  allStrategies,
) where

import Data.List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)

import Snake.Core.Grid (Direction (..), isOutOfBounds, nextPosition)
import Snake.Core.State (GameState (..), snakeBody)

-- | A bot strategy takes the current state and returns the next direction to take.
-- If it returns Nothing, don't change the direction.
type BotStrategy = GameState -> Maybe Direction

allStrategies :: [(String, BotStrategy)]
allStrategies =
  [ ("naive_bot", naiveStrategy)
  , ("greedy_bot", greedyStrategy)
  ]

naiveStrategy :: GameState -> Maybe Direction
naiveStrategy GameState{target, snakeHead}
  | snakeHeadX < targetX = Just RIGHT
  | snakeHeadY < targetY = Just DOWN
  | snakeHeadX > targetX = Just LEFT
  | snakeHeadY > targetY = Just UP
  | otherwise = Nothing
  where
    (targetX, targetY) = target
    (snakeHeadX, snakeHeadY) = snakeHead

greedyStrategy :: GameState -> Maybe Direction
greedyStrategy state@GameState{gameGrid, target, snakeHead} =
  fmap fst . listToMaybe $
    sortBy (comparing $ \(dir, _) -> if isProductive dir then 0 else 1 :: Int) $
      filter isValid allOptions
  where
    allOptions =
      map
        (\dir -> (dir, nextPosition dir snakeHead))
        [minBound .. maxBound]
    isValid (_, next) =
      not $ isOutOfBounds gameGrid next || next `elem` snakeBody state
    isProductive dir =
      case dir of
        LEFT -> snakeHeadX > targetX
        UP -> snakeHeadY > targetY
        RIGHT -> snakeHeadX < targetX
        DOWN -> snakeHeadY < targetY

    (snakeHeadX, snakeHeadY) = snakeHead
    (targetX, targetY) = target