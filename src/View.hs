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


cellSize, canvasWidth, canvasHeight :: Int
cellSize = 30
canvasWidth = boardNj * cellSize
canvasHeight = boardNi * cellSize

cellSizeD, canvasWidthD, canvasHeightD :: Double
cellSizeD = fromIntegral cellSize
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
  let x0 = fromIntegral (j*cellSize)
      y0 = fromIntegral (i*cellSize)
  fillStyle (color $ Style.Hex "BBBBBB")
  fillRect (x0, y0, cellSizeD, cellSizeD)
 
