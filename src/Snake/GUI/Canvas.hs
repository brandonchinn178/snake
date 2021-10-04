{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Snake.GUI.Canvas (
  -- * Board
  Board (..),
  BoardHeight,
  BoardWidth,
  mkBoard,

  -- * A pixel on the canvas
  Pixel (..),
  pixelTopLeft,
  pixelTopRight,
  pixelBottomRight,
  pixelBottomLeft,
  pixelCenter,

  -- * Converesions
  coordinateToPixel,
) where

import Graphics.UI.Threepenny qualified as UI

import Snake.Core.Grid (Coordinate, Grid (..))

data Board = Board
  { boardHeight :: BoardHeight
  , boardWidth :: BoardWidth
  , pixelSize :: Double
  }

type BoardHeight = Int
type BoardWidth = Int

-- | Create a board to match the same aspect ratio as the grid.
mkBoard :: Grid -> BoardHeight -> BoardWidth -> Board
mkBoard Grid{..} maxBoardHeight maxBoardWidth =
  Board
    { boardHeight = pixelSize * gridHeight
    , boardWidth = pixelSize * gridWidth
    , pixelSize = fromIntegral pixelSize
    }
  where
    pixelSize =
      floor $
        min @Double
          (fromIntegral maxBoardWidth / fromIntegral gridWidth)
          (fromIntegral maxBoardHeight / fromIntegral gridHeight)

data Pixel = Pixel
  { pixelLeft :: Double
  , pixelTop :: Double
  , pixelRight :: Double
  , pixelBottom :: Double
  } deriving (Show)

pixelTopLeft :: Pixel -> UI.Point
pixelTopLeft Pixel{..} = (pixelLeft, pixelTop)

pixelTopRight :: Pixel -> UI.Point
pixelTopRight Pixel{..} = (pixelRight, pixelTop)

pixelBottomRight :: Pixel -> UI.Point
pixelBottomRight Pixel{..} = (pixelRight, pixelBottom)

pixelBottomLeft :: Pixel -> UI.Point
pixelBottomLeft Pixel{..} = (pixelLeft, pixelBottom)

pixelCenter :: Pixel -> UI.Point
pixelCenter Pixel{..} = ((pixelLeft + pixelRight) / 2, (pixelTop + pixelBottom) / 2)

-- | Convert a Coordinate on the snake grid to a pixel on the canvas.
coordinateToPixel :: Board -> Coordinate -> Pixel
coordinateToPixel Board{pixelSize} (x, y) =
  let pixelLeft = pixelSize * fromIntegral x
      pixelTop = pixelSize * fromIntegral y
      pixelRight = pixelLeft + pixelSize
      pixelBottom = pixelTop + pixelSize
   in Pixel{..}
