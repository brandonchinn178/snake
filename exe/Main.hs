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
  port <- (fromMaybe 8023 . join . fmap readMaybe) <$> lookupEnv "PORT"

  let config = defaultConfig
        { jsAddr = Just host
        , jsPort = Just port
        }
      opts = GameOptions
        { gameGrid =
            Grid
              { gridWidth = 40
              , gridHeight = 40
              }
        }

  when shouldOpenBrowser $
    void $ openBrowser $ "http://" ++ Char8.unpack host ++ ":" ++ show port

  startGUI config (gui opts)
