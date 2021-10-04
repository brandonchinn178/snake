{-# LANGUAGE NamedFieldPuns #-}

module Snake.GUI.Manager (
  GameManager (..),
  initManager,
  reinitManager,

  -- * Game information
  getMillisPerFrame,
  getScore,
) where

import Snake.Core.State (GameState (..), mkInitialState)
import Snake.Core.Targets (NextTargets, mkNextTargets)

data GameManager = GameManager
  { gameState :: GameState
  , nextTargets :: NextTargets
  }

initManager :: IO GameManager
initManager = initManagerWith <$> mkNextTargets

reinitManager :: GameManager -> GameManager
reinitManager = initManagerWith . nextTargets

initManagerWith :: NextTargets -> GameManager
initManagerWith nextTargets =
  let (gameState, nextTargets') = mkInitialState nextTargets
   in GameManager
        { gameState = gameState
        , nextTargets = nextTargets'
        }

{-- Game state --}

getMillisPerFrame :: GameManager -> Int
getMillisPerFrame GameManager{gameState = GameState{snakeTail}} =
  let level = length snakeTail `div` targetsPerLevel
   in max lowestMillisPerFrame $ initialMillisPerFrame + (level * changePerLevel)
  where
    initialMillisPerFrame = 150
    lowestMillisPerFrame = 10
    -- change in ms/frame per level
    changePerLevel = -25
    -- how many targets to consume before incrementing the level
    targetsPerLevel = 3

getScore :: GameManager -> Int
getScore = length . snakeTail . gameState
