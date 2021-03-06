{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Snake.Strategy (
  BotStrategy,
  allStrategies,
) where

import Control.DeepSeq (NFData)
import Control.Parallel.Strategies (rdeepseq, rparWith, parMap)
import GHC.Generics (Generic)
import Data.Either (fromRight)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, listToMaybe)

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

allStrategies :: [(String, Int -> BotStrategy)]
allStrategies =
  [ ("naive_bot", naiveStrategy)
  , ("greedy_bot", greedyStrategy)
  , ("lookahead_bot", lookaheadStrategy)
  ]

{-- Naive strategy --}

naiveStrategy :: Int -> BotStrategy
naiveStrategy _ GameState{target, snakeHead}
  | snakeHeadX < targetX = Just RIGHT
  | snakeHeadY < targetY = Just DOWN
  | snakeHeadX > targetX = Just LEFT
  | snakeHeadY > targetY = Just UP
  | otherwise = Nothing
  where
    (targetX, targetY) = target
    (snakeHeadX, snakeHeadY) = snakeHead

{-- Greedy strategy --}

greedyStrategy :: Int -> BotStrategy
greedyStrategy _ GameState{..} =
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
  . nTimes numSteps (concat . parMap (rparWith rdeepseq) getNextStates)
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
          snakeLen = len snakeBody
          len = fromIntegral . length
          mTarget = either (const Nothing) Just targetOrCount
          mCount = either (Just . fromIntegral) (const Nothing) targetOrCount
          orZero = fromMaybe 0
       in sum
            [ snakeLen
            , (* 10) $ orZero $ recip <$> mCount
            , (* 10) $ case mTarget of
                Nothing ->
                  -- make sure this is always higher than the Right branch
                  2
                Just target ->
                  -- manhattanDistance >= 1 because targetOrCount is Left
                  -- when target == snakeHead
                  recip $ manhattanDistance target snakeHead ** 2
                  -- TODO: incentivize being on same x/y coordinate as target
                  -- TODO: why does snake keep going down?
            , let numKinks = len . filter isKink . triples $ snakeBody
               in 1 - numKinks / snakeLen
              -- TODO: incentivize moving away from body
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
  } deriving (Generic, NFData)

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

manhattanDistance :: Num a => Coordinate -> Coordinate -> a
manhattanDistance (x1, y1) (x2, y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)

getAllValidDirections :: Grid -> Coordinate -> [Direction] -> [Direction]
getAllValidDirections gameGrid snakeHead snakeTail = filter isValid [minBound .. maxBound]
  where
    isValid dir =
      let next = nextPosition dir snakeHead
       in not $ isOutOfBounds gameGrid next || next `elem` getSnakeBody snakeHead snakeTail
