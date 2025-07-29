{-# LANGUAGE OverloadedStrings #-}

module Update where

import Miso

import Model

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action 
  = ActionReset

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect Model Action
updateModel ActionReset = io_ (consoleLog "TODO reset")
