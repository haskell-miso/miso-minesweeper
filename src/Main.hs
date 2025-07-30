
import Control.Monad.IO.Class (liftIO)
import Miso
import System.Random (getStdGen)

import Model
import Update
import View

import Miso.Lens
import Game

main :: IO ()
main = run $ do
  gen0 <- getStdGen
  model <- liftIO (mkModel gen0)
  let model = model0 & mGame .~ g
  startComponent 
    (component model updateModel viewModel) 
      { events = defaultEvents <> pointerEvents   -- TODO mouseEvents
      , logLevel = DebugAll
      }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

