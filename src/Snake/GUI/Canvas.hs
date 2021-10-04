{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Snake.GUI.Canvas (
  -- * Canvas dimensions
  canvasHeight,
  canvasWidth,

  -- * A pixel on the canvas
  Pixel (..),
  pixelTopLeft,
  pixelTopRight,
  pixelBottomRight,
  pixelBottomLeft,
  pixelCenter,
  getPixelWidth,
  getPixelHeight,

  -- * Converesions
  coordinateToPixel,
) where

import Graphics.UI.Threepenny qualified as UI

import Snake.Core.Grid (Coordinate, Grid (..))

canvasHeight :: Num a => a
canvasHeight = 500

canvasWidth :: Num a => a
canvasWidth = 500

getPixelWidth :: Grid -> Double
getPixelWidth Grid{gridWidth} = canvasWidth / fromIntegral gridWidth

getPixelHeight :: Grid -> Double
getPixelHeight Grid{gridHeight} = canvasHeight / fromIntegral gridHeight

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
coordinateToPixel :: Grid -> Coordinate -> Pixel
coordinateToPixel grid (x, y) =
  let pixelLeft = pixelWidth * fromIntegral x
      pixelTop = pixelHeight * fromIntegral y
      pixelRight = pixelLeft + pixelWidth
      pixelBottom = pixelTop + pixelHeight
   in Pixel{..}
  where
    pixelWidth = getPixelWidth grid
    pixelHeight = getPixelHeight grid
