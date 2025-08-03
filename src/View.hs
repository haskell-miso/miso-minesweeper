{-# LANGUAGE OverloadedStrings #-}

module View where

import Control.Monad (forM_, when)
import Miso
import Miso.Canvas as Canvas
import Miso.Lens
import Miso.String (ms)
import Miso.Style qualified as Style

import Game
import Helpers
import Model
import Update

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

cellSizeD :: Double
cellSizeD = fromIntegral cellSize

cs007, cs01, cs02, cs03, cs04, cs05, cs06, cs07, cs08, cs09 :: Double
cs007 = cellSizeD * 0.07
cs01 = cellSizeD * 0.1
cs02 = cellSizeD * 0.2
cs03 = cellSizeD * 0.3
cs04 = cellSizeD * 0.4
cs05 = cellSizeD * 0.5
cs06 = cellSizeD * 0.6
cs07 = cellSizeD * 0.7
cs08 = cellSizeD * 0.8
cs09 = cellSizeD * 0.9

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: Model -> View Model Action
viewModel model = div_ [] 
  [ h1_ [] [ "miso-minesweeper" ]
  , p_ [] 
      [ button_ [ onClick (ActionAskReset ModeBeginner) ]     [ "beginner" ]
      , button_ [ onClick (ActionAskReset ModeIntermediate) ] [ "intermediate" ]
      , button_ [ onClick (ActionAskReset ModeExpert) ]       [ "expert" ]
      ]
  , p_ [] [ "left-click to discover, middle-click to flag/unflag" ]
  , Canvas.canvas 
      [ width_ (ms $ nj * cellSize)
      , height_ (ms $ ni * cellSize)
      , Style.style_  [Style.border "2px solid black"]
      , onPointerUp ActionAskPlay
      ]
    initCanvas
    (drawCanvas model)
  , p_ [] 
        [ text ("status: " <> fmtStatus (model ^. mGame ^. gStatus))
        , br_ []
        , text ("flags: " <> ms (show $ model ^. mGame ^. gFlags))
        , br_ []
        , text ("mines: " <> ms (show $ model ^. mGame ^. gNbMines))
        , br_ []
        , text ("remaining cells: " <> ms (show $ model ^. mGame ^. gRemCells))
        ]
  ]
  where
    (ni, nj) = model ^. mGame ^. gBoardNiNj

    fmtStatus = \case
      StatusRunning   -> "running"
      StatusWon       -> "won"
      StatusLost      -> "lost"

-------------------------------------------------------------------------------
-- canvas
-------------------------------------------------------------------------------

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Model -> () -> Canvas ()
drawCanvas model () = do
  let (ni, nj) = model ^. mGame ^. gBoardNiNj
      w = fromIntegral $ nj * cellSize
      h = fromIntegral $ ni * cellSize
  clearRect (0, 0, w, h)
  font cellFont
  drawBackground w h
  forGame (model ^. mGame) drawGameCell
  drawGrid ni nj w h

-------------------------------------------------------------------------------
-- drawing functions
-------------------------------------------------------------------------------

drawGrid :: Int -> Int -> Double -> Double -> Canvas ()
drawGrid ni nj w h = do
  fillStyle (color Style.black)
  beginPath ()
  forM_ [1 .. nj-1] $ \j -> do
    let x = fromIntegral (j * cellSize)
    moveTo (x, 0)
    lineTo (x, h)
  forM_ [1 .. ni-1] $ \i -> do
    let y = fromIntegral (i * cellSize)
    moveTo (0, y)
    lineTo (w, y)
  stroke ()

drawBackground :: Double -> Double -> Canvas ()
drawBackground w h = do
  fillStyle (color colorNo)
  fillRect (0, 0, w, h)

drawCell :: Style.Color -> Canvas ()
drawCell c = do
  fillStyle (color c)
  fillRect (0, 0, cellSizeD, cellSizeD)

drawMine :: Bool -> Int -> Int -> Canvas ()
drawMine wrong i j = do

  save ()
  translate $ ij2xy i j

  when wrong $ drawCell colorWrongMine

  beginPath ()
  moveTo (cs02, cs02)
  lineTo (cs08, cs08)
  moveTo (cs02, cs08)
  lineTo (cs08, cs02)
  moveTo (cs05, cs01)
  lineTo (cs05, cs09)
  moveTo (cs01, cs05)
  lineTo (cs09, cs05)
  stroke ()

  fillStyle (color Style.black)
  beginPath ()
  arc (cs05, cs05, cs03, 0, 2*pi)
  fill ()

  fillStyle (color Style.white)
  beginPath ()
  arc (cs04, cs04, cs007, 0, 2*pi)
  fill ()

  restore ()

drawFlag :: Bool -> Int -> Int -> Canvas ()
drawFlag wrong i j = do

  save ()
  translate $ ij2xy i j

  when wrong $ drawCell colorWrongFlag

  fillStyle (color Style.red)
  beginPath ()
  moveTo (cs02, cs04)
  lineTo (cs06, cs02)
  lineTo (cs06, cs06)
  closePath ()
  fill ()

  fillStyle (color Style.black)
  fillRect (cs06, cs02, cs01, cs06)

  restore ()

drawFree :: Int -> Int -> Int -> Canvas ()
drawFree i j n = do
  save ()
  translate $ ij2xy i j
  drawCell colorYes
  fillStyle (color $ n2color n)
  when (n > 0) $ fillText (ms (show n), cs03, cs08)
  restore ()

drawGameCell :: Int -> Int -> Cell -> Canvas ()
drawGameCell i j = \case
  CellUnknown -> pure ()
  CellFree n  -> drawFree i j n
  CellFlag    -> drawFlag False i j
  CellFlagKo  -> drawFlag True i j
  CellMine    -> drawMine False i j
  CellMineKo  -> drawMine True i j

