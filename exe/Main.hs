{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (join, void, when)
import Data.ByteString.Char8 qualified as Char8
import Data.Maybe (fromMaybe, isNothing)
import Graphics.UI.Threepenny.Core (Config (..), defaultConfig, startGUI)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Browser (openBrowser)

import Snake (GameOptions (..), Grid (..), gui)

main :: IO ()
main = do
  shouldOpenBrowser <- isNothing <$> lookupEnv "NO_OPEN_BROWSER"

  let host = "localhost"
  port <- fromMaybe 8023 <$> readEnv "PORT"

  initialMillisPerFrame <- fromMaybe 150 <$> readEnv "INITIAL_MS_PER_FRAME"
  gridWidth <- fromMaybe 40 <$> readEnv "GRID_WIDTH"
  gridHeight <- fromMaybe 40 <$> readEnv "GRID_HEIGHT"
  maxBoardHeight <- fromMaybe 500 <$> readEnv "MAX_BOARD_HEIGHT"
  maxBoardWidth <- fromMaybe 500 <$> readEnv "MAX_BOARD_WIDTH"

  let config = defaultConfig
        { jsAddr = Just host
        , jsPort = Just port
        }
      opts = GameOptions
        { initialMillisPerFrame
        , gameGrid = Grid{gridWidth, gridHeight}
        , maxBoardHeight
        , maxBoardWidth
        }

  when shouldOpenBrowser $
    void $ openBrowser $ "http://" ++ Char8.unpack host ++ ":" ++ show port

  startGUI config (gui opts)

readEnv :: Read a => String -> IO (Maybe a)
readEnv = fmap (join . fmap readMaybe) . lookupEnv
