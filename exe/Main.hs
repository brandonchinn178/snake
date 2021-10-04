{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard, join, msum, void, when)
import Data.ByteString.Char8 qualified as Char8
import Data.Maybe (fromMaybe, isNothing)
import Graphics.UI.Threepenny.Core (Config (..), defaultConfig, startGUI)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Browser (openBrowser)

import Snake (
  GameMode (..),
  GameOptions (..),
  Grid (..),
  gui,
 )
import Snake.Strategy (allStrategies)

main :: IO ()
main = do
  shouldOpenBrowser <- isNothing <$> lookupEnv "NO_OPEN_BROWSER"

  let host = "localhost"
  port <- fromMaybe 8023 <$> readEnv "PORT"

  gameMode <- parseMode . fromMaybe "interactive" =<< lookupEnv "MODE"
  initialFPS <- fromMaybe 10 <$> readEnv "INITIAL_FPS"
  gridWidth <- fromMaybe 40 <$> readEnv "GRID_WIDTH"
  gridHeight <- fromMaybe 40 <$> readEnv "GRID_HEIGHT"
  maxBoardHeight <- fromMaybe 500 <$> readEnv "MAX_BOARD_HEIGHT"
  maxBoardWidth <- fromMaybe 500 <$> readEnv "MAX_BOARD_WIDTH"

  let config = defaultConfig
        { jsAddr = Just host
        , jsPort = Just port
        }
      opts = GameOptions
        { gameMode
        , initialFPS
        , gameGrid = Grid{gridWidth, gridHeight}
        , maxBoardHeight
        , maxBoardWidth
        }

  when shouldOpenBrowser $
    void $ openBrowser $ "http://" ++ Char8.unpack host ++ ":" ++ show port

  startGUI config (gui opts)

readEnv :: Read a => String -> IO (Maybe a)
readEnv = fmap (join . fmap readMaybe) . lookupEnv

parseMode :: String -> IO GameMode
parseMode mode =
  maybe (error $ "Unknown mode: " ++ mode) return $
    msum
      [ guard (mode == "interactive") *> Just Interactive
      , RunBot <$> lookup mode allStrategies
      ]
