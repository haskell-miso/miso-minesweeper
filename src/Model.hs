
module Model where

import Control.Monad.Primitive
import Miso.Lens
import Miso.Lens.TH
import System.Random.Stateful

import Game
import Helpers

data Model = Model
  { _mGame :: Game
  , _mGen :: StdGen
  } deriving (Eq)

makeLenses ''Model

mkModel :: (PrimMonad m) => Mode -> StdGen -> m Model
mkModel mode gen0 = uncurry Model <$> runStateGenT gen0 (mkGame $ mode2infos mode)

resetModel :: (PrimMonad m) => Mode -> Model -> m Model
resetModel mode = mkModel mode . _mGen

