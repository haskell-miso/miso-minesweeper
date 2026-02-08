module Maine (main) where

import Control.Monad.IO.Class (liftIO)
import Miso
import System.Random (getStdGen)

import Helpers
import Model
import Update
import View

main :: IO ()
main = do
  gen <- getStdGen
  model <- liftIO (mkModel ModeBeginner gen)
  startApp
    (component model updateModel viewModel) 
      { events = defaultEvents <> pointerEvents
      -- , logLevel = DebugAll
      }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

