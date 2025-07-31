{-# LANGUAGE OverloadedStrings #-}

module Update where

import Control.Monad.IO.Class (liftIO)
import Miso
import Miso.Lens
import Miso.String (ms)

import Game
import Helpers
import Model

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action 
  = ActionAskReset
  | ActionAskPlay PointerEvent
  | ActionSetModel Model
  | ActionSetGame Game

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect Model Action

updateModel ActionAskReset = do
  model <- get
  io (ActionSetModel <$> liftIO (resetModel model))

updateModel (ActionSetModel model) = 
  put model

updateModel (ActionSetGame game) = 
  mGame .= game

updateModel (ActionAskPlay pointer) = do
  let (i, j) = uncurry xy2ij $ client pointer
      move = MoveFree i j   -- TODO
  game <- use mGame
  io_ (consoleLog ("playFree " <> ms (show i) <> " " <> ms (show j)))
  io (ActionSetGame <$> liftIO (play move game))

