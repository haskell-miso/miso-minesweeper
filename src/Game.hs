
module Game
  ( boardNi
  , boardNj
  , Game(..)
  ) where

import Data.Massiv.Array as A
import System.Random.Stateful

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
  { gFlags :: BitMap
  , gMines :: BitMap
  }

