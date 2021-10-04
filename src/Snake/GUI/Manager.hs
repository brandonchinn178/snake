{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Snake.GUI.Manager (
  GameManager (..),
  initManager,
  reinitManager,

  -- * Game information
  getMillisPerFrame,
  getScore,
) where

import Snake.Core.State (GameState, mkInitialState)
import Snake.Core.State qualified as GameState (GameState (..))
import Snake.Core.Targets (NextTargets, mkNextTargets)
import Snake.GUI.Canvas (Board, mkBoard)
import Snake.GUI.Options (GameOptions (..))

data GameManager = GameManager
  { gameOptions :: GameOptions
  , gameBoard :: Board
  , gameState :: GameState
  , nextTargets :: NextTargets
  }

initManager :: GameOptions -> IO GameManager
initManager opts@GameOptions{gameGrid} = initManagerWith opts <$> mkNextTargets gameGrid

reinitManager :: GameManager -> GameManager
reinitManager GameManager{gameOptions, nextTargets} =
  initManagerWith gameOptions nextTargets

initManagerWith :: GameOptions -> NextTargets -> GameManager
initManagerWith opts@GameOptions{..} nextTargets =
  let (gameState, nextTargets') = mkInitialState gameGrid nextTargets
   in GameManager
        { gameOptions = opts
        , gameBoard = mkBoard gameGrid maxBoardHeight maxBoardWidth
        , gameState = gameState
        , nextTargets = nextTargets'
        }

{-- Game state --}

getMillisPerFrame :: GameManager -> Int
getMillisPerFrame GameManager{gameOptions, gameState} =
  let level = length (GameState.snakeTail gameState) `div` targetsPerLevel
   in max lowestMillisPerFrame $ initialMillisPerFrame + (level * changePerLevel)
  where
    GameOptions{initialMillisPerFrame} = gameOptions
    lowestMillisPerFrame = 10
    -- change in ms/frame per level
    changePerLevel = -25
    -- how many targets to consume before incrementing the level
    targetsPerLevel = 3

getScore :: GameManager -> Int
getScore = length . GameState.snakeTail . gameState