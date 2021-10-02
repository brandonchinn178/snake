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

gui :: Window -> UI ()
gui window = do
  {-- Game state --}

  initialState <- liftIO mkInitialState
  (updateStateEvent, addStateUpdate) <- liftIO newEvent
  stateBehavior <- accumB initialState updateStateEvent

  {-- DOM setup --}

  set' title "Snake" window
  body <- getBody window

  canvas <- UI.canvas
  applyAll_ canvas
    [ set' UI.height canvasHeight
    , set' UI.width canvasWidth
    , set' style [("border", "solid black 1px")]
    ]
  appendTo body [canvas]

  {-- Event handling --}

  timer <- UI.timer & sink UI.interval (millisPerFrame <$> stateBehavior)
  on UI.tick timer $ \_ -> do
    UI.clearCanvas canvas
    state <- currentValue stateBehavior
    drawFrame state canvas

  on UI.keydown body $ \c -> do
    let setMovementTo movement = liftIO $ addStateUpdate $ \state -> state{snakeMovement = Just movement}
    case keyFromCode c of
      Just LeftArrow -> setMovementTo LEFT
      Just UpArrow -> setMovementTo UP
      Just RightArrow -> setMovementTo RIGHT
      Just DownArrow -> setMovementTo DOWN
      Nothing -> return ()

  UI.start timer

drawFrame :: GameState -> UI.Canvas -> UI ()
drawFrame state@GameState{target} canvas = do
  -- draw snake
  set' UI.fillStyle snakeColor canvas
  forM_ (snakeBody state) $ \snakePart -> do
    let snakePartBox = coordinateToPixel snakePart
    applyAll_ canvas
      [ UI.beginPath
      , UI.fillRect (pixelTopLeft snakePartBox) pixelWidth pixelHeight
      , UI.closePath
      , UI.fill
      ]

  -- draw target
  let targetBox = coordinateToPixel target
  applyAll_ canvas
    [ set' UI.fillStyle targetColor
    , UI.beginPath
    , fillCircle (pixelCenter targetBox) (pixelWidth / 2)
    , UI.closePath
    , UI.fill
    ]
  where
    snakeColor = UI.htmlColor "black"
    targetColor = UI.htmlColor "red"
    fillCircle center radius = UI.arc center radius 0 (2 * pi)

{-- Keybindings --}

data Key
  = LeftArrow
  | UpArrow
  | RightArrow
  | DownArrow
  deriving (Show, Eq)

keyFromCode :: Int -> Maybe Key
keyFromCode = \case
  37 -> Just LeftArrow
  38 -> Just UpArrow
  39 -> Just RightArrow
  40 -> Just DownArrow
  _ -> Nothing

{-- Utilities --}

applyAll :: Monad m => a -> [a -> m b] -> m [b]
applyAll a = mapM ($ a)

applyAll_ :: Monad m => a -> [a -> m b] -> m ()
applyAll_ a fs = void $ applyAll a fs

appendTo :: Element -> [Element] -> UI ()
appendTo p cs = void $ pure p #+ map pure cs
