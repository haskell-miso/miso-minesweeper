
import Control.Monad.IO.Class (liftIO)
import Miso
import System.Random (getStdGen)

import Model
import Update
import View

main :: IO ()
main = run $ do
  gen0 <- getStdGen
  model <- liftIO $ mkModel gen0
  startComponent 
    (component model updateModel viewModel) 
      { events = defaultEvents <> mouseEvents
      , logLevel = DebugAll
      }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

