
import Control.Monad.IO.Class (liftIO)
import Miso
import System.Random (getStdGen)

-- import Game
import Model
import Update
import View

{-
import System.Random
import System.Random.Stateful

test :: IO ()
test = do
  gen <- getStdGen
  b <- runStateGenT_ gen mkMines
  print b
-}

main :: IO ()
main = run $ do
  gen0 <- getStdGen
  model <- liftIO $ mkModel gen0
  startComponent 
    (component model updateModel viewModel) 
      { logLevel = DebugAll
      }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif


