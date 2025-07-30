
module Game
  ( Cell(..)
  , Game
  , Status(..)
  , boardNi
  , boardNj
  , forGame
  , gStatus
  , mkGame
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

-- create random mines, using rejection sampling
mkMines :: (StatefulGen g m, PrimMonad m) => g -> m (Array U Ix2 Bool)
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

data Status
  = StatusRunning
  | StatusWon
  | StatusLost
  deriving (Eq)

data Cell
  = CellUnknown
  | CellFree Int
  | CellFlag
  | CellFlagKo
  | CellMine
  | CellMineKo
  deriving (Eq)

data Game = Game
  { _gMines       :: Array U Ix2 Bool
  , _gNeighbors   :: Array U Ix2 Int
  , _gCells       :: Array D Ix2 Cell
  , _gStatus      :: Status
  } deriving (Eq)

makeLenses ''Game

mkGame :: (StatefulGen g m, PrimMonad m) => g -> m Game
mkGame gen = do
  mines <- mkMines gen
  let neighbors = A.replicate (ParOn []) boardSize 0   -- TODO
  let cells = A.replicate (ParOn []) boardSize CellUnknown
  let game = Game mines neighbors cells StatusRunning
  let game' = game & gCells .~ updateCellsLost game   -- TODO
  pure game'

play :: PrimMonad m => Int -> Int -> Game -> m Game
play _ _ = pure -- TODO

forGame :: (Monad m) => Game -> (Int -> Int -> Cell -> m ()) -> m ()
forGame game f = A.iforM_ (game ^. gCells) $ \(Ix2 i j) c -> f i j c

updateCellsLost :: Game -> Array D Ix2 Cell   -- TODO
updateCellsLost game = 
  flip A.map (game ^. gMines) $ \mine ->
    if mine then CellMine else CellUnknown

