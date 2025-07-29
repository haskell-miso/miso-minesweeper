
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Miso

import Game
import Model
import Update
import View

import System.Random
import System.Random.Stateful

main :: IO ()
main = do
  gen <- getStdGen
  b <- runStateGenT_ gen newBoard
  print b

{-
main :: IO ()
main = run $ do
  let model = Model
      app = component model updateModel viewModel
  startComponent app
    { logLevel = DebugAll
    }
-}

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif


