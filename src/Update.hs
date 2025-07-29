
module Update where

import Miso

import Model

data Action = Action

updateModel :: Action -> Effect Model Action
updateModel _ = pure ()
