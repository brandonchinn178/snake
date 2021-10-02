module Snake (gui) where

import Control.Monad (void)
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Core

gui :: Window -> UI ()
gui window = do
  pure window
    # set title "Snake"
    # void

  getBody window
    #+
      [ UI.p # set text "Hello world"
      ]
    # void
