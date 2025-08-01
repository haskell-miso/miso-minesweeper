
module Game
  ( Cell(..)
  , Game
  , Move(..)
  , Status(..)
  , forGame
  , gBoardNiNj
  , gFlags
  , gNbMines
  , gRemCells
  , gStatus
  , mkGame
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
  , _gBoardNiNj   :: (Int, Int)
  , _gNbMines     :: Int
  } deriving (Eq)

makeLenses ''Game

-------------------------------------------------------------------------------
-- export
-------------------------------------------------------------------------------

mkGame :: (StatefulGen g m, PrimMonad m) => (Int, Int, Int) -> g -> m Game
mkGame (ni, nj, nbMines) gen = do
  mines <- mkMines (ni, nj, nbMines) gen
  let neighbors = computeNeighbors mines
      cells = A.replicate (ParOn []) (Sz2 ni nj) CellUnknown
      remainingCells = ni * nj - nbMines
      game = Game mines neighbors cells StatusRunning 0 remainingCells (ni, nj) nbMines
  pure game

forGame :: (Monad m) => Game -> (Int -> Int -> Cell -> m ()) -> m ()
forGame game f = A.iforM_ (game ^. gCells) $ \(Ix2 i j) c -> f i j c

play :: (PrimMonad m) => Move -> Game -> m Game
play (MoveFlag i j) = execStateT (playFlag i j) 
play (MoveFree i j) = execStateT (playFree i j) 

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

-- create random mines, using rejection sampling
mkMines :: (StatefulGen g m, PrimMonad m) => (Int, Int, Int) -> g -> m (Array U Ix2 Bool)
mkMines (ni, nj, nbMines) gen = do
  arr <- newMArray (Sz2 ni nj) False
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

computeNeighbors :: Array U Ix2 Bool -> Array P Ix2 Int
computeNeighbors mines = 
  let 
    f = bool 0 1
    count3x3Stencil = makeStencil (Sz (3 :. 3)) (1 :. 1) $ \w ->
      f(w (-1 :. -1)) + f(w (-1 :. 0)) + f(w (-1 :. 1)) +
      f(w ( 0 :. -1)) +                  f(w ( 0 :. 1)) +
      f(w ( 1 :. -1)) + f(w ( 1 :. 0)) + f(w ( 1 :. 1)) 
  in compute $ mapStencil (Fill False) count3x3Stencil mines

playFlag :: (MonadState Game m, PrimMonad m) => Int -> Int -> m ()
playFlag i j = do
  let ij = Ix2 i j
  cells <- thawS @B @Ix2 @Cell =<< use gCells
  c <- A.read cells ij
  case c of
    Just CellUnknown -> do
      write_ cells ij CellFlag
      gFlags += 1
    Just CellFlag -> do
      write_ cells ij CellUnknown
      gFlags -= 1
    _ -> pure ()
  freezeS cells >>= assign gCells

playFree :: (MonadState Game m, PrimMonad m) => Int -> Int -> m ()
playFree i j = do
  status <- use gStatus
  when (status == StatusRunning) $ do
    let ij = Ix2 i j
    c <- (`index` ij) <$> use gCells 
    when (c /= Just CellFlag) $ do
      m <- (`index` ij) <$> use gMines 
      case m of
        Just True -> playFreeKo ij
        Just False ->  playFreeOk ij
        Nothing -> pure ()

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
  discoverCells ij
  -- check win
  rc <- use gRemCells
  when (rc == 0) $ do
    -- show flags in cells
    gCells %= compute . A.map (\c -> if c == CellUnknown then CellFlag else c)
    -- update status
    gStatus .= StatusWon

-- assumes ij0 is a free cell (no mine)
discoverCells :: (MonadState Game m, PrimMonad m) => Ix2 -> m ()
discoverCells ij0' = do
  cells <- thawS @B @Ix2 @Cell =<< use gCells
  (ni, nj) <- use gBoardNiNj
  neighbors <- use gNeighbors

  let
    discover [] = pure ()
    discover (ij0:ijs) = do
      mc0 <- A.read cells ij0
      case mc0 of
        Nothing -> discover ijs
        Just c0 -> 
          if c0 /= CellUnknown
            then discover ijs
            else do
              -- unknown cell -> discover
              let n = neighbors ! ij0
              write_ cells ij0 (CellFree n)
              gRemCells -= 1
              if n > 0
                then discover ijs
                else do
                  -- no neighboring mine -> try neighboring cells
                  let (Ix2 i0 j0) = ij0
                      newIjs = [ Ix2 i j 
                               | i<-[i0-1 .. i0+1], j<-[j0-1 .. j0+1]
                               , i/=i0 || j/=j0   -- not the same ij
                               , i>=0 && i<ni, j>=0 && j<nj   -- not outside
                               ]
                  discover (newIjs ++ ijs)

  discover [ij0']

  freezeS cells >>= assign gCells

