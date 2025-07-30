
module Model where

import Control.Monad.Primitive
import Miso.Lens
import Miso.Lens.TH
import System.Random.Stateful

import Game

data Model = Model
  { _mGame :: Game
  , _mGen :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: (PrimMonad m) => StdGen -> m Model
mkModel gen0 = do
  (game, gen) <- runStateGenT gen0 mkGame
  pure (Model game gen)

resetModel :: (PrimMonad m) => Model -> m Model
resetModel model = mkModel (model ^. mGen)

-- play

