
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

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect Model Action

updateModel ActionAskReset = do
  model <- get
  io (ActionReset <$> liftIO (resetModel model))

updateModel (ActionReset model) = 
  put model

