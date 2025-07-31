{-# LANGUAGE TypeApplications #-}

module Game
  ( Cell(..)
  , Game
  , Move(..)
  , Status(..)
  , boardNi
  , boardNj
  , forGame
  , gFlags
  , gRemCells
  , gStatus
  , mkGame
  , nbMines
  , play
  ) where

import Control.Monad (when)
import Control.Monad.State
import Data.Bool (bool)
import Data.Massiv.Array as A
import Miso.Lens
import Miso.Lens.TH
import System.Random.Stateful (uniformRM, StatefulGen)

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

-- easy
boardNi, boardNj, nbMines :: Int
boardNi = 9
boardNj = 9
nbMines = 10

{-
-- hard
boardNi, boardNj, nbMines :: Int
boardNi = 16
boardNj = 30
nbMines = 99
-}

boardSize :: Sz Ix2
boardSize = Sz2 boardNi boardNj

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
  , _gFlags       :: Int
  , _gRemCells    :: Int
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
      remainingCells = boardNi * boardNj - nbMines
      game = Game mines neighbors cells StatusRunning 0 remainingCells
  pure game

forGame :: (Monad m) => Game -> (Int -> Int -> Cell -> m ()) -> m ()
forGame game f = A.iforM_ (game ^. gCells) $ \(Ix2 i j) c -> f i j c

play :: (PrimMonad m) => Move -> Game -> m Game
play (MoveFlag i j) = execStateT (playFlag i j) 
play (MoveFree i j) = execStateT (playFree i j) 

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

-- TODO implement playFlag
playFlag :: (MonadState Game m, PrimMonad m) => Int -> Int -> m ()
playFlag i j = pure ()

playFree :: (MonadState Game m, PrimMonad m) => Int -> Int -> m ()
playFree i j = do
  status <- use gStatus
  when (status == StatusRunning && isValidIJ i j) $ do
    let ij = Ix2 i j
    m <- (! ij) <$> use gMines 
    if m
      then playFreeKo ij
      else playFreeOk ij

playFreeKo :: (MonadState Game m, PrimMonad m) => Ix2 -> m ()
playFreeKo ij = do
  -- update cells
  cells <- thawS @B @Ix2 @Cell =<< use gCells
  write_ cells ij CellMineKo
  freezeS cells >>= assign gCells
  -- show mines and flags in cells
  mines <- use gMines
  gCells %= compute . A.zipWith upCell mines
  -- update status
  gStatus .= StatusLost
  where
    upCell False CellFlag = CellFlagKo
    upCell True CellUnknown = CellMine
    upCell _ c = c

playFreeOk :: (MonadState Game m, PrimMonad m) => Ix2 -> m ()
playFreeOk ij = do
  -- update cells
  n <- (! ij) <$> use gNeighbors 
  cells <- thawS @B @Ix2 @Cell =<< use gCells
  write_ cells ij (CellFree n)
  freezeS cells >>= assign gCells
  gRemCells -= 1
  -- TODO discover free cells 0
  -- check win
  rc <- use gRemCells
  when (rc == 0) $ do
    -- show flags in cells
    gCells %= compute . A.map (\c -> if c == CellUnknown then CellFlag else c)
    -- update status
    gStatus .= StatusWon

