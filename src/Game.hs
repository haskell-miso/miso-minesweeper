{-# LANGUAGE TypeApplications #-}

module Game
  ( Cell(..)
  , Game
  , Move(..)
  , Status(..)
  , boardNi
  , boardNj
  , forGame
  , gRemaining
  , gStatus
  , mkGame
  , play
  ) where

import Data.Bool (bool)
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
-- types
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

data Move
  = MoveFree Int Int
  | MoveFlag Int Int

data Game = Game
  { _gMines       :: Array U Ix2 Bool
  , _gNeighbors   :: Array P Ix2 Int
  , _gCells       :: Array B Ix2 Cell
  , _gStatus      :: Status
  , _gRemaining   :: Int
  } deriving (Eq)

makeLenses ''Game

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

mkGame :: (StatefulGen g m, PrimMonad m) => g -> m Game
mkGame gen = do
  mines <- mkMines gen
  let neighbors = computeNeighbors mines
      cells = A.replicate (ParOn []) boardSize CellUnknown
      game = Game mines neighbors cells StatusRunning nbMines
  pure game

forGame :: (Monad m) => Game -> (Int -> Int -> Cell -> m ()) -> m ()
forGame game f = A.iforM_ (game ^. gCells) $ \(Ix2 i j) c -> f i j c

play :: PrimMonad m => Move -> Game -> m Game
play (MoveFlag i j) game = 
  pure game    -- TODO
play (MoveFree i j) game = 
  if isValidIJ i j && game ^. gStatus == StatusRunning
    then do
      let ij = Ix2 i j
          m = game ^. gMines ! ij
          n = game ^. gNeighbors ! ij
      cells0 <- thawS @B @Ix2 @Cell (game ^. gCells)
      if m
        then write_ cells0 (Ix2 i j) CellMine
        else write_ cells0 (Ix2 i j) (CellFree n)
      cells1 <- freezeS cells0
      pure (game & gCells .~ cells1)
    else pure game

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

isValidIJ :: Int -> Int -> Bool
isValidIJ i j = i>=0 && i<boardNi && j>=0 && j<boardNj

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

computeNeighbors :: Array U Ix2 Bool -> Array P Ix2 Int
computeNeighbors mines = 
  let 
    f = bool 0 1
    count3x3Stencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \w ->
      f(w (-1 :. -1)) + f(w (-1 :. 0)) + f(w (-1 :. 1)) +
      f(w ( 0 :. -1)) +                  f(w ( 0 :. 1)) +
      f(w ( 1 :. -1)) + f(w ( 1 :. 0)) + f(w ( 1 :. 1)) 
  in compute $ mapStencil (Fill False) count3x3Stencil mines

updateCellsLost :: Game -> Array D Ix2 Cell   -- TODO
updateCellsLost game = A.zipWith f (game ^. gMines) (game ^. gNeighbors)
  where
    f mine cell = case (mine, cell) of
      (True, _) -> CellMine
      (_, x) -> CellFree x

