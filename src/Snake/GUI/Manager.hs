{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Snake.GUI.Manager (
  GameManager (..),
  initManager,
  reinitManager,

  -- * Game information
  getFramesPerSecond,
  getScore,
  getNextManagerState,
) where

import Snake.Core.State (
  GameState,
  getNextState,
  mkInitialState,
  setMovement,
 )
import Snake.Core.State qualified as GameState (GameState (..))
import Snake.Core.Targets (NextTargets, mkNextTargets)
import Snake.GUI.Canvas (Board, mkBoard)
import Snake.GUI.Options (GameMode (..), GameOptions (..))

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

getFramesPerSecond :: GameManager -> Int
getFramesPerSecond GameManager{gameOptions, gameState} =
  let level = length (GameState.snakeTail gameState) `div` targetsPerLevel
   in round $ fromIntegral initialFPS * fpsMultiplier ** fromIntegral level
  where
    GameOptions{initialFPS} = gameOptions
    -- fps multiplier per level
    fpsMultiplier = 1.3 :: Double
    -- how many targets to consume before incrementing the level
    targetsPerLevel = 3

getScore :: GameManager -> Int
getScore = length . GameState.snakeTail . gameState

getNextManagerState :: GameManager -> GameManager
getNextManagerState manager@GameManager{..} =
  manager
    { gameState = gameState''
    , nextTargets = nextTargets'
    }
  where
    (gameState'', nextTargets') = getNextState gameState' nextTargets
    gameState' =
      case gameMode gameOptions of
        Interactive -> gameState
        RunBot getNextDirection
          | Just dir <- getNextDirection gameState -> setMovement gameState dir
        _ -> gameState
