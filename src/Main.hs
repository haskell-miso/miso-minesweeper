
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Miso

import Game
import Model
import Update
import View

main :: IO ()
main = run $ do
  let model = Model
      app = component model updateModel viewModel
  startComponent app
    { logLevel = DebugAll
    }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif


