{-# LANGUAGE OverloadedStrings #-}

module Update where

import Control.Monad.IO.Class (liftIO)
import Miso

import Model

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action 
  = ActionAskReset
  | ActionReset Model
  | ActionAskUp

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect Model Action

updateModel ActionAskReset = do
  model <- get
  io (ActionReset <$> liftIO (resetModel model))

updateModel (ActionReset model) = 
  put model

updateModel ActionAskUp = 
  io_ (consoleLog "TODO button up")

