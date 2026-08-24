{-# LANGUAGE CPP #-}

module Main (main) where

import Miso
import System.Random (getStdGen)

import Helpers
import Model
import Update
import View

main :: IO ()
main = do
  gen <- getStdGen
  model <- mkModel ModeBeginner gen
  startApp (defaultEvents <> pointerEvents) (component model updateModel viewModel)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
