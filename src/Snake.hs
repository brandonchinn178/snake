{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Snake (
  gui,
  module Snake.GUI.Options,
) where

import Control.Monad (forM_, void)
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core

import Snake.Core.Grid
import Snake.Core.State
import Snake.GUI.Canvas
import Snake.GUI.Keys
import Snake.GUI.Manager
import Snake.GUI.Options

gui :: GameOptions -> Window -> UI ()
gui opts window = mdo
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
      & sink UI.height (boardHeight . gameBoard <$> managerB)
      & sink UI.width (boardWidth . gameBoard <$> managerB)
      & set style
          [ ("border-style", "solid")
          , ("border-width", "3px")
          ]
      & sink (mkWriteAttr drawGame) managerB

  appendTo body
    [ scoreBox
    , canvas
    ]

  {-- Timer setup --}

  let toMillisPerFrame fps = round (1000 / fromIntegral fps :: Double)
  timer <-
    UI.timer
      & sink UI.interval (toMillisPerFrame . getFramesPerSecond <$> managerB)
      & run UI.start

  {-- Game manager --}

  initialManager <- liftIO $ initManager opts
  let managerUpdateE =
        fmap concatenate . unions $
          [ UI.tick timer $> getNextManagerState
          , UI.keydown body <&> \c ->
              let setMovementTo movement = \manager@GameManager{..} ->
                    case gameMode opts of
                      Interactive -> manager{gameState = setMovement gameState movement}
                      _ -> manager
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
          ]
  managerB <- accumB initialManager managerUpdateE

  return ()

drawGame :: GameManager -> UI.Canvas -> UI ()
drawGame GameManager{..} canvas = do
  UI.clearCanvas canvas

  -- draw snake
  element canvas
    & set UI.fillStyle snakeColor
    & void
  forM_ (snakeBody gameState) $ \snakePart -> do
    let snakePartBox = coordinateToPixel gameBoard snakePart
    element canvas
      & runAll
          [ UI.beginPath
          , UI.fillRect (pixelTopLeft snakePartBox) pixelSize pixelSize
          , UI.closePath
          , UI.fill
          ]

  -- draw target
  let targetBox = coordinateToPixel gameBoard target
  element canvas
    & set UI.fillStyle targetColor
    & runAll
        [ UI.beginPath
        , fillCircle (pixelCenter targetBox) (pixelSize / 2)
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
      let centerX = fromIntegral boardWidth / 2
          startY = fromIntegral boardHeight * 2/5
      element canvas
        & set UI.fillStyle (UI.htmlColor "#ffffffaa")
        & runAll
            [ UI.beginPath
            , UI.fillRect (0, 0) (fromIntegral boardWidth) (fromIntegral boardHeight)
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
    Board{boardHeight, boardWidth, pixelSize} = gameBoard
    GameState{gameStatus, target} = gameState
    snakeColor = UI.htmlColor "#025d8c"
    targetColor = UI.htmlColor "#ffdd00"
    fillCircle center radius = UI.arc center radius 0 (2 * pi)

{-- Utilities --}

run :: Monad m => (a -> m ()) -> m a -> m a
run f ma = do
  a <- ma
  f a
  return a

runAll :: Monad m => [a -> m ()] -> m a -> m a
runAll fs ma = do
  a <- ma
  mapM_ ($ a) fs
  return a

appendTo :: Element -> [Element] -> UI ()
appendTo p cs = void $ pure p #+ map pure cs
