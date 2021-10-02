{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Snake (gui) where

import Control.Monad (void)
import Data.Function ((&))
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core

gui :: Window -> UI ()
gui window = do
  set' title "Snake" window
  body <- getBody window

  canvas <-
    UI.canvas
      & set UI.height canvasSize
      & set UI.width canvasSize
      & set style [("border", "solid black 1px")]
  body <+ [canvas]

  let millisPerFrame = 1000
  timer <- UI.timer & set UI.interval millisPerFrame
  on UI.tick timer $ \_ -> debug "tick"

  on UI.keydown body $ \c ->
    case keyFromCode c of
      Just key -> debug (show key)
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
