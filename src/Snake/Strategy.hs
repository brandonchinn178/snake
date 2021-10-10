{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Snake.Strategy (
  BotStrategy,
  allStrategies,
) where

import Data.Either (fromRight)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)

import Snake.Core.Grid (
  Coordinate,
  Direction (..),
  Grid,
  isOutOfBounds,
  nextPosition,
 )
import Snake.Core.State (
  GameState (..),
  getNextSnakePosition,
  getSnakeBody,
 )

-- | A bot strategy takes the current state and returns the next direction to take.
-- If it returns Nothing, don't change the direction.
type BotStrategy = GameState -> Maybe Direction

allStrategies :: [(String, BotStrategy)]
allStrategies =
  [ ("naive_bot", naiveStrategy)
  , ("greedy_bot", greedyStrategy)
  , ("lookahead_bot", lookaheadStrategy 5)
  ]

{-- Naive strategy --}

naiveStrategy :: BotStrategy
naiveStrategy GameState{target, snakeHead}
  | snakeHeadX < targetX = Just RIGHT
  | snakeHeadY < targetY = Just DOWN
  | snakeHeadX > targetX = Just LEFT
  | snakeHeadY > targetY = Just UP
  | otherwise = Nothing
  where
    (targetX, targetY) = target
    (snakeHeadX, snakeHeadY) = snakeHead

{-- Greedy strategy --}

greedyStrategy :: BotStrategy
greedyStrategy GameState{..} =
  listToMaybe $
    sortOn (\dir -> if isProductive dir then 0 else 1 :: Int) $
      getAllValidDirections gameGrid snakeHead snakeTail
  where
    isProductive dir =
      case dir of
        LEFT -> snakeHeadX > targetX
        UP -> snakeHeadY > targetY
        RIGHT -> snakeHeadX < targetX
        DOWN -> snakeHeadY < targetY

    (snakeHeadX, snakeHeadY) = snakeHead
    (targetX, targetY) = target

{-- Lookahead strategy --}

-- | Look ahead the given number of steps and return the best option.
lookaheadStrategy :: Int -> BotStrategy
lookaheadStrategy numSteps _ | numSteps <= 0 =
  error "lookahead_bot needs to look at least one step ahead"
lookaheadStrategy numSteps initialGameState@GameState{gameGrid} =
  fmap (last . moveHistory)
  . maximumOn rankState
  . nTimes numSteps (concatMap getNextStates)
  $ [getInitialLookaheadState initialGameState]
  where
    getNextStates :: LookaheadState -> [LookaheadState]
    getNextStates LookaheadState{..} =
      flip map (getAllValidDirections gameGrid snakeHead snakeTail) $ \dir ->
        let -- if we already ate the target, take it off the grid
            target = fromRight (-1, -1) targetOrCount
            (snakeHead', snakeTail', gotTarget) = getNextSnakePosition snakeHead snakeTail dir target
         in LookaheadState
              { moveHistory = dir : moveHistory
              , snakeHead = snakeHead'
              , snakeTail = snakeTail'
              , targetOrCount =
                  if gotTarget
                    then Left (length moveHistory)
                    else targetOrCount
              }

    -- rank the given state, the higher the better
    rankState :: LookaheadState -> Double
    rankState LookaheadState{..} =
      let snakeBody = getSnakeBody snakeHead snakeTail
          whenLeft e f = either f (const 0) e
          whenRight e f = either (const 0) f e
          inv = recip . fromIntegral
       in sum
            [ (* 10) . fromIntegral $ length snakeBody
            , (* 10) . whenLeft targetOrCount $ \count ->
                -- add 1 so it's always higher than manhattan distance
                1 + inv count
            , (* 10) . whenRight targetOrCount $ \target ->
                -- manhattanDistance >= 1 because targetOrCount is Left
                -- when target == snakeHead
                inv $ manhattanDistance target snakeHead
            , inv $ (length . filter isKink . triples) snakeBody + 1
            ]

    isKink :: (Coordinate, Coordinate, Coordinate) -> Bool
    isKink ((x1, y1), (x2, y2), (x3, y3)) =
      not $ (x1 == x2 && x2 == x3) || (y1 == y2 && y2 == y3)

data LookaheadState = LookaheadState
  { moveHistory :: [Direction]
  , snakeHead :: Coordinate
  , snakeTail :: [Direction]
  , targetOrCount :: Either Int Coordinate
    -- ^ The target or, if the target was eaten, how many
    -- steps until the target was eaten
  }

getInitialLookaheadState :: GameState -> LookaheadState
getInitialLookaheadState GameState{..} =
  LookaheadState
    { moveHistory = []
    , snakeHead
    , snakeTail
    , targetOrCount = Right target
    }

{-- Helpers --}

nTimes :: Int -> (a -> a) -> a -> a
nTimes n _ _ | n < 0 = error "nTimes called with negative number"
nTimes n f a = iterate f a !! n

maximumOn :: Ord b => (a -> b) -> [a] -> Maybe a
maximumOn _ [] = Nothing
maximumOn f xs = Just (last $ sortOn f xs)

triples :: [a] -> [(a, a, a)]
triples xs
  | length xs < 3 = []
  | otherwise = zip3 xs (tail xs) (tail $ tail xs)

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getAllValidDirections :: Grid -> Coordinate -> [Direction] -> [Direction]
getAllValidDirections gameGrid snakeHead snakeTail = filter isValid [minBound .. maxBound]
  where
    isValid dir =
      let next = nextPosition dir snakeHead
       in not $ isOutOfBounds gameGrid next || next `elem` getSnakeBody snakeHead snakeTail
