{-# LANGUAGE OverloadedStrings #-}

module View where

import Control.Monad (forM_)
import Miso
import Miso.Canvas as Canvas
import Miso.Lens
import Miso.String (MisoString, ms)
import Miso.Style qualified as Style

import Game
import Model
import Update

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

cellSize :: Int
cellSize = 30

cellFont :: MisoString
cellFont = "small-caps bold 25px arial"

canvasWidth, canvasHeight :: Int
canvasWidth = boardNj * cellSize
canvasHeight = boardNi * cellSize

cellSizeD, canvasWidthD, canvasHeightD :: Double
cellSizeD = fromIntegral cellSize
canvasWidthD = fromIntegral canvasWidth
canvasHeightD = fromIntegral canvasHeight

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

viewModel :: Model -> View Action
viewModel model = div_ [] 
  [ h1_ [] [ "miso-minesweeper" ]
  , Canvas.canvas 
      [ width_ (ms canvasWidth)
      , height_ (ms canvasHeight)
      , Style.style_  [Style.border "2px solid black"]
      ]
    initCanvas
    (drawCanvas model)
  , p_ [] [ "left-click to discover, middle-click to flag/unflag" ]
  , p_ [] [ text ("status: " <> fmtStatus (model ^. mGame ^. gStatus)) ]
  , p_ [] [ text ("remaining: " <> (ms $ show $ model ^. mGame ^. gRemaining)) ]
  , p_ [] 
      [ button_ 
        [ onClick ActionAskReset ]
        [ text "reset" ]
      ]
  ]
  where
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
  clearRect (0, 0, canvasWidthD, canvasHeightD)
  drawBackground
  forGame (model ^. mGame) drawGameCell
  drawFlag 2 1    -- TODO
  drawGrid

-------------------------------------------------------------------------------
-- drawing functions
-------------------------------------------------------------------------------

drawGrid :: Canvas ()
drawGrid = do
  fillStyle (color Style.black)
  beginPath ()
  forM_ [1 .. boardNj-1] $ \j -> do
    let x = fromIntegral (j * cellSize)
    moveTo (x, 0)
    lineTo (x, canvasHeightD)
  forM_ [1 .. boardNi-1] $ \i -> do
    let y = fromIntegral (i * cellSize)
    moveTo (0, y)
    lineTo (canvasWidthD, y)
  stroke ()

drawBackground :: Canvas ()
drawBackground = do
  fillStyle (color $ Style.Hex "DDDDDD")
  fillRect (0, 0, canvasWidthD, canvasHeightD)

drawMine :: Int -> Int -> Canvas ()
drawMine i j = do

  save ()
  translate $ ij2xy i j

  fillStyle (color $ Style.Hex "BBBBBB")
  fillRect (0, 0, cellSizeD, cellSizeD)

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

drawFlag :: Int -> Int -> Canvas ()
drawFlag i j = do

  save ()
  translate $ ij2xy i j

  fillStyle (color Style.red)
  beginPath ()
  moveTo (cs02, cs04)
  lineTo (cs06, cs02)
  lineTo (cs06, cs06)
  closePath ()
  fill ()

  fillStyle (color Style.black)
  fillRect (cs06, cs01, cs01, cs08)
  fillRect (cs04, cs08, cs05, cs01)

  restore ()

drawFree :: Int -> Int -> Int -> Canvas ()
drawFree _ _ 0 = pure ()
drawFree i j n = do
  save ()
  translate $ ij2xy i j
  fillStyle (color $ n2color n)
  font cellFont
  fillText (ms (show n), cs02, cs08)
  restore ()

drawGameCell :: Int -> Int -> Cell -> Canvas ()   -- TODO
drawGameCell i j = \case
  CellMine -> drawMine i j
  CellFree n -> drawFree i j n
  _ -> pure ()


ij2xy :: Int -> Int -> (Double, Double)
ij2xy i j = (fromIntegral (j*cellSize), fromIntegral (i*cellSize))

n2color :: Int -> Style.Color
n2color = \case
  1 -> Style.Hex "0000FF"
  2 -> Style.Hex "007B00"
  3 -> Style.Hex "FF0000"
  4 -> Style.Hex "00007B"
  5 -> Style.Hex "7B0000"
  _ -> Style.black

