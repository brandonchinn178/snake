{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (join)
import Data.ByteString.Char8 qualified as Char8
import Data.Maybe (fromMaybe)
import Graphics.UI.Threepenny.Core (Config (..), defaultConfig, startGUI)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Browser (openBrowser)

import Snake (gui)

main :: IO ()
main = do
  let host = "localhost"
  port <- (fromMaybe 8023 . join . fmap readMaybe) <$> lookupEnv "PORT"

  let config = defaultConfig
        { jsAddr = Just host
        , jsPort = Just port
        }

  _ <- openBrowser $ "http://" ++ Char8.unpack host ++ ":" ++ show port
  startGUI config gui
