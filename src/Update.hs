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
  = ActionAskReset Mode
  | ActionAskPlay PointerEvent
  | ActionSetModel Model
  | ActionSetGame Game

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect Model Action

updateModel (ActionAskReset mode) = do
  model <- get
  io (ActionSetModel <$> liftIO (resetModel mode model))

updateModel (ActionSetModel model) = 
  put model

updateModel (ActionSetGame game) = 
  mGame .= game

updateModel (ActionAskPlay event) = do
  let (i, j) = uncurry xy2ij $ offset event 
  game <- use mGame
  case button event of
    0 -> do
      io_ (consoleLog ("playFree " <> ms (show i) <> " " <> ms (show j)))
      io (ActionSetGame <$> liftIO (play (MoveFree i j) game))
    1 -> do
      io_ (consoleLog ("playFlag " <> ms (show i) <> " " <> ms (show j)))
      io (ActionSetGame <$> liftIO (play (MoveFlag i j) game))
    _ -> pure ()

