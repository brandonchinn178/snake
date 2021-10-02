{-# LANGUAGE LambdaCase #-}

module Snake (gui) where

import Control.Monad (void)
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

  canvas <-
    UI.canvas
      & set UI.height canvasSize
      & set UI.width canvasSize
      & set style [("border", "solid black 1px")]
  body <+ [canvas]

  {-- Event handling --}

  timer <- UI.timer & sink UI.interval (millisPerFrame <$> stateBehavior)
  on UI.tick timer $ \_ -> do
    UI.clearCanvas canvas
    state <- currentValue stateBehavior
    UI.strokeText (show state) (0, 100) canvas

  on UI.keydown body $ \c -> do
    let setMovementTo movement = liftIO $ addStateUpdate $ \state -> state{snakeMovement = movement}
    case keyFromCode c of
      Just LeftArrow -> setMovementTo MoveLeft
      Just UpArrow -> setMovementTo MoveUp
      Just RightArrow -> setMovementTo MoveRight
      Just DownArrow -> setMovementTo MoveDown
      Nothing -> return ()

  UI.start timer

{-- Constants --}

canvasSize :: Int
canvasSize = 500

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

{-- Aliases --}

(<+) :: Element -> [Element] -> UI ()
p <+ cs = void $ pure p #+ map pure cs
