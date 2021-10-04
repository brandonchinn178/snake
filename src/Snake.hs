{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Snake (gui) where

import Control.Monad (forM_, void)
import Data.Function ((&))
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core

import Snake.Core.Grid
import Snake.Core.State
import Snake.GUI.Canvas
import Snake.GUI.Keys
import Snake.GUI.Manager

gui :: Window -> UI ()
gui window = do
  {-- Game manager --}

  initialManager <- liftIO initManager
  (managerUpdateEvent, addManagerUpdate) <- liftIO newEvent
  managerB <- accumB initialManager managerUpdateEvent

  {-- DOM setup --}

  set' title "Snake" window
  body <-
    getBody window
      & set style
          [ ("display", "grid")
          , ("align-items", "center")
          , ("justify-content", "center")
          , ("font-size", "18px")
          ]

  scoreBox <-
    UI.p
      & sink UI.text (("Score: " <>) . show . getScore <$> managerB)
      & set style
          [ ("text-align", "center")
          , ("font-weight", "bold")
          ]

  canvas <-
    UI.canvas
      & set UI.height canvasHeight
      & set UI.width canvasWidth
      & set style
          [ ("border-style", "solid")
          , ("border-width", "3px")
          ]
      & sink (mkWriteAttr (drawState . gameState)) managerB

  appendTo body
    [ scoreBox
    , canvas
    ]

  {-- Event handling --}

  timer <-
    UI.timer
      & sink UI.interval (getMillisPerFrame <$> managerB)

  on UI.tick timer $ \_ -> liftIO $ addManagerUpdate $
    \manager@GameManager{..} ->
      let (gameState', nextTargets') = getNextState gameState nextTargets
       in manager
            { gameState = gameState'
            , nextTargets = nextTargets'
            }

  on UI.keydown body $ \c -> liftIO . addManagerUpdate $
    let setMovementTo movement = \manager@GameManager{..} ->
          manager
            { gameState =
                if isRunning (gameStatus gameState)
                  then gameState{gameStatus = SnakeHissingTowards movement}
                  else gameState
            }
        restartGame = \manager@GameManager{..} ->
          if isRunning (gameStatus gameState)
            then manager
            else reinitManager manager
     in case keyFromCode c of
          Nothing -> id
          -- change direction: arrow keys
          Just LeftArrow -> setMovementTo LEFT
          Just UpArrow -> setMovementTo UP
          Just RightArrow -> setMovementTo RIGHT
          Just DownArrow -> setMovementTo DOWN
          -- change direction: ijkl
          Just LetterJ -> setMovementTo LEFT
          Just LetterI -> setMovementTo UP
          Just LetterL -> setMovementTo RIGHT
          Just LetterK -> setMovementTo DOWN
          -- change direction: wasd
          Just LetterA -> setMovementTo LEFT
          Just LetterW -> setMovementTo UP
          Just LetterD -> setMovementTo RIGHT
          Just LetterS -> setMovementTo DOWN
          -- restart game
          Just SpaceBar -> restartGame

  UI.start timer

drawState :: GameState -> UI.Canvas -> UI ()
drawState state@GameState{gameStatus, target} canvas = do
  UI.clearCanvas canvas

  -- draw snake
  element canvas
    & set UI.fillStyle snakeColor
    & void
  forM_ (snakeBody state) $ \snakePart -> do
    let snakePartBox = coordinateToPixel snakePart
    element canvas
      & runAll
          [ UI.beginPath
          , UI.fillRect (pixelTopLeft snakePartBox) pixelWidth pixelHeight
          , UI.closePath
          , UI.fill
          ]

  -- draw target
  let targetBox = coordinateToPixel target
  element canvas
    & set UI.fillStyle targetColor
    & runAll
        [ UI.beginPath
        , fillCircle (pixelCenter targetBox) (pixelWidth / 2)
        , UI.closePath
        , UI.fill
        ]
    & void

  -- draw failure message
  let failureMessage =
        case gameStatus of
          SnakeRanIntoWall -> Just "You ran into the wall!"
          SnakeAteItself -> Just "You ran into yourself!"
          _ -> Nothing
  case failureMessage of
    Nothing -> return ()
    Just msg -> do
      let centerX = canvasWidth / 2
          startY = canvasHeight * 2/5
      element canvas
        & set UI.fillStyle (UI.htmlColor "#ffffffaa")
        & runAll
            [ UI.beginPath
            , UI.fillRect (0, 0) canvasWidth canvasHeight
            , UI.closePath
            , UI.fill
            ]
        & set UI.fillStyle (UI.htmlColor "#a80000")
        & set UI.textFont "24px sans-serif"
        & set UI.textAlign UI.Center
        & runAll
            [ UI.fillText msg (centerX, startY)
            ]
        & set UI.fillStyle (UI.htmlColor "black")
        & set UI.textFont "18px sans-serif"
        & runAll
            [ UI.fillText "Hit the SPACE bar to restart." (centerX, startY + 50)
            ]
        & void
  where
    snakeColor = UI.htmlColor "#025d8c"
    targetColor = UI.htmlColor "#ffdd00"
    fillCircle center radius = UI.arc center radius 0 (2 * pi)

{-- Utilities --}

runAll :: Monad m => [a -> m ()] -> m a -> m a
runAll fs ma = do
  a <- ma
  mapM_ ($ a) fs
  return a

appendTo :: Element -> [Element] -> UI ()
appendTo p cs = void $ pure p #+ map pure cs
