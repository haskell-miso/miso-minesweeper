
module Game
  ( boardNi
  , boardNj
  , Game(..)
  , mkGame
  , getMines
  ) where

import Data.Massiv.Array as A
import Miso.Lens
import Miso.Lens.TH
import System.Random.Stateful (uniformRM, StatefulGen)

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

boardNi, boardNj :: Int
boardNi = 16
boardNj = 30

boardSize :: Sz Ix2
boardSize = Sz2 boardNi boardNj

nbMines :: Int
nbMines = 99

-------------------------------------------------------------------------------
-- BitMap
-------------------------------------------------------------------------------

type BitMap = Array U Ix2 Bool

-- create a random BitMap, using rejection sampling
mkMines :: (StatefulGen g m, PrimMonad m) => g -> m BitMap
mkMines gen = do
  arr <- newMArray boardSize False
  let go 0 = pure ()
      go n = do
        ij <- Ix2 <$> uniformRM (0, boardNi-1) gen 
                  <*> uniformRM (0, boardNj-1) gen
        c <- A.read arr ij
        case c of
          Just False -> write_ arr ij True >> go (n-1)
          _ -> go n
  go nbMines
  freezeS arr

-------------------------------------------------------------------------------
-- Game
-------------------------------------------------------------------------------

data Game = Game
  { _gMines :: BitMap
  -- , _gFlags :: BitMap
  } deriving (Eq)

makeLenses ''Game

mkGame :: (StatefulGen g m, PrimMonad m) => g -> m Game
mkGame gen = Game <$> mkMines gen

getMines :: Game -> [(Int, Int)]
getMines game = ifoldMono f (game ^. gMines)
  where 
    f (Ix2 i j) = \case
      True -> [(i, j)]
      False -> []

