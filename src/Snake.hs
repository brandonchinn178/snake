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
  (stateEvent, setState) <- liftIO newEvent
  stateBehavior <- stepper initialState stateEvent
  let updateState f = updateStateM (pure . f)
      updateStateM f = do
        state <- currentValue stateBehavior
        liftIO $ setState =<< f state

  {-- DOM setup --}

  set' title "Snake" window
  body <-
    getBody window
      & set style
          [ ("display", "grid")
          , ("align-items", "center")
          , ("justify-content", "center")
          , ("font-size", "16px")
          ]

  scoreBox <-
    UI.p
      & sink UI.text (("Score: " <>) . show . getScore <$> stateBehavior)
      & set style
          [ ("text-align", "center")
          , ("font-style", "bold")
          ]

  canvas <-
    UI.canvas
      & set UI.height canvasHeight
      & set UI.width canvasWidth
      & set style [("border", "solid black 1px")]

  appendTo body
    [ scoreBox
    , canvas
    ]

  {-- Event handling --}

  timer <-
    UI.timer
      & sink UI.interval (getMillisPerFrame <$> stateBehavior)

  on UI.tick timer $ \_ -> do
    UI.clearCanvas canvas
    state <- liftIO . getNextState =<< currentValue stateBehavior
    liftIO $ setState state
    drawFrame state canvas

  on UI.keydown body $ \c -> do
    let setMovementTo movement = updateState $ \state ->
          if isRunning (gameStatus state)
            then state{gameStatus = SnakeHissingTowards movement}
            else state
        restartGame = updateStateM $ \state ->
          if isRunning (gameStatus state)
            then pure state
            else mkInitialState

    case keyFromCode c of
      Nothing -> return ()
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

drawFrame :: GameState -> UI.Canvas -> UI ()
drawFrame state@GameState{gameStatus, target} canvas = do
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

{-- Game state --}

getMillisPerFrame :: GameState -> Int
getMillisPerFrame GameState{snakeTail} =
  let level = length snakeTail `div` targetsPerLevel
   in max lowestMillisPerFrame $ initialMillisPerFrame + (level * changePerLevel)
  where
    initialMillisPerFrame = 150
    lowestMillisPerFrame = 10
    -- change in ms/frame per level
    changePerLevel = -25
    -- how many targets to consume before incrementing the level
    targetsPerLevel = 3

getScore :: GameState -> Int
getScore = length . snakeTail

{-- Keybindings --}

data Key
  = LeftArrow
  | UpArrow
  | RightArrow
  | DownArrow
  | LetterA
  | LetterD
  | LetterI
  | LetterJ
  | LetterK
  | LetterL
  | LetterS
  | LetterW
  | SpaceBar
  deriving (Show, Eq)

keyFromCode :: Int -> Maybe Key
keyFromCode = \case
  32 -> Just SpaceBar
  37 -> Just LeftArrow
  38 -> Just UpArrow
  39 -> Just RightArrow
  40 -> Just DownArrow
  65 -> Just LetterA
  68 -> Just LetterD
  73 -> Just LetterI
  74 -> Just LetterJ
  75 -> Just LetterK
  76 -> Just LetterL
  83 -> Just LetterS
  87 -> Just LetterW
  _ -> Nothing

{-- Utilities --}

runAll :: Monad m => [a -> m ()] -> m a -> m a
runAll fs ma = do
  a <- ma
  mapM_ ($ a) fs
  return a

appendTo :: Element -> [Element] -> UI ()
appendTo p cs = void $ pure p #+ map pure cs
