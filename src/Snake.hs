{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Snake (gui) where

import Control.Monad (forM_, void)
import Data.Function ((&))
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core

import Snake.Grid
import Snake.State

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
    debug (show state)

    forM_ [(x, y) | x <- [0..gridWidth - 1], y <- [0..gridHeight - 1]] $ \coord -> do
      let box = coordinateToBox coord
      applyAll_ canvas
        [ set' UI.fillStyle $ UI.htmlColor "#ddd"
        , set' UI.strokeStyle "#666"
        , UI.beginPath
        , UI.moveTo $ topLeft box
        , UI.lineTo $ topRight box
        , UI.lineTo $ bottomRight box
        , UI.lineTo $ bottomLeft box
        , UI.closePath
        , UI.stroke
        , UI.beginPath
        , UI.arc (boxCenter box) 2 0 (2 * pi)
        , UI.closePath
        , UI.fill
        ]

  on UI.keydown body $ \c -> do
    let setMovementTo movement = liftIO $ addStateUpdate $ \state -> state{snakeMovement = movement}
    case keyFromCode c of
      Just LeftArrow -> setMovementTo MoveLeft
      Just UpArrow -> setMovementTo MoveUp
      Just RightArrow -> setMovementTo MoveRight
      Just DownArrow -> setMovementTo MoveDown
      Nothing -> return ()

  UI.start timer

{-- Canvas --}

canvasHeight :: Int
canvasHeight = 500

canvasWidth :: Int
canvasWidth = 500

data CanvasBox = CanvasBox
  { leftBound :: Double
  , topBound :: Double
  , rightBound :: Double
  , bottomBound :: Double
  } deriving (Show)

topLeft :: CanvasBox -> UI.Point
topLeft CanvasBox{..} = (leftBound, topBound)

topRight :: CanvasBox -> UI.Point
topRight CanvasBox{..} = (rightBound, topBound)

bottomRight :: CanvasBox -> UI.Point
bottomRight CanvasBox{..} = (rightBound, bottomBound)

bottomLeft :: CanvasBox -> UI.Point
bottomLeft CanvasBox{..} = (leftBound, bottomBound)

boxCenter :: CanvasBox -> UI.Point
boxCenter CanvasBox{..} = ((leftBound + rightBound) / 2, (topBound + bottomBound) / 2)

-- | Convert a Coordinate on the snake grid to a box on the canvas.
coordinateToBox :: Coordinate -> CanvasBox
coordinateToBox (x, y) =
  let leftBound = boxWidth * fromIntegral x
      topBound = boxHeight * fromIntegral y
      rightBound = leftBound + boxWidth
      bottomBound = topBound + boxHeight
   in CanvasBox{..}
  where
    boxWidth = fromIntegral canvasWidth / fromIntegral gridWidth
    boxHeight = fromIntegral canvasHeight / fromIntegral gridHeight

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
