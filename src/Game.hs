
module Game where

import Data.Massiv.Array as A
import System.Random.Stateful

boardSize :: Sz Ix2
boardSize = Sz2 16 30

nbMines :: Int
nbMines = 99

type Board = Array U Ix2 Bool

-- create a new board, using rejection sampling
newBoard :: (StatefulGen g m, PrimMonad m) => g -> m Board
newBoard gen = do
  let Sz2 ni nj = boardSize
  arr <- newMArray boardSize False
  let go 0 = pure ()
      go n = do
        ij <- Ix2 <$> uniformRM (0, ni-1) gen 
                  <*> uniformRM (0, nj-1) gen
        c <- A.read arr ij
        case c of
          Just False -> write_ arr ij True >> go (n-1)
          _ -> go n
  go nbMines
  freezeS arr


