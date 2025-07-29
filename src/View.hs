{-# LANGUAGE OverloadedStrings #-}

module View where

import Control.Monad (forM_)
import Miso
import Miso.Canvas as Canvas
import Miso.Lens
import Miso.String (ms)
import Miso.Style qualified as Style

import Game
import Model
import Update

-------------------------------------------------------------------------------
-- params
-------------------------------------------------------------------------------

cellSize :: Int
cellSize = 30

mineSizeD :: Double
mineSizeD = 9

canvasWidth, canvasHeight :: Int
canvasWidth = boardNj * cellSize
canvasHeight = boardNi * cellSize

cellSizeD, cellSize05D, canvasWidthD, canvasHeightD :: Double
cellSizeD = fromIntegral cellSize
cellSize05D = cellSizeD * 0.5
canvasWidthD = fromIntegral canvasWidth
canvasHeightD = fromIntegral canvasHeight

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
  , p_ [] [ "remaining mines: " ]
  , p_ [] 
      [ button_ 
        [ onClick ActionAskReset ]
        [ text "reset" ]
      ]
  ]

-------------------------------------------------------------------------------
-- canvas
-------------------------------------------------------------------------------

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Model -> () -> Canvas ()
drawCanvas model () = do
  clearRect (0, 0, canvasWidthD, canvasHeightD)

  drawBackground

  forM_ (model ^. mGame & getMines) drawMine

  -- drawMine 10 20

  drawGrid

drawGrid :: Canvas ()
drawGrid = do
  fillStyle (color Style.black)
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

drawMine :: (Int, Int) -> Canvas ()
drawMine (i, j) = do

  save ()

  let x0 = fromIntegral (j*cellSize)
      y0 = fromIntegral (i*cellSize)
  translate (x0, y0)

  fillStyle (color $ Style.Hex "BBBBBB")
  fillRect (0, 0, cellSizeD, cellSizeD)

  let cs02 = cellSizeD * 0.2
      cs08 = cellSizeD * 0.8
      cs01 = cellSizeD * 0.1
      cs09 = cellSizeD * 0.9
  beginPath ()
  moveTo (cs02, cs02)
  lineTo (cs08, cs08)
  moveTo (cs02, cs08)
  lineTo (cs08, cs02)
  moveTo (cellSize05D, cs01)
  lineTo (cellSize05D, cs09)
  moveTo (cs01, cellSize05D)
  lineTo (cs09, cellSize05D)
  stroke ()

  fillStyle (color Style.black)
  beginPath ()
  arc (cellSize05D, cellSize05D, mineSizeD, 0, 2*pi)
  fill ()

  fillStyle (color Style.white)
  beginPath ()
  arc (cellSizeD*0.4, cellSizeD*0.4, 2, 0, 2*pi)
  fill ()

  restore ()
 
