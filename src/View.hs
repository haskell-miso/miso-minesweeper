{-# LANGUAGE OverloadedStrings #-}

module View where

import Control.Monad (forM_)
import Miso
import Miso.Canvas as Canvas
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

canvasWidthD, canvasHeightD :: Double
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
      , Style.style_  [Style.border "1px solid black"]
      ]
    initCanvas
    (drawCanvas model)
  , p_ [] [ "remaining mines: " ]
  , p_ [] 
      [ button_ 
        [ onClick ActionReset ]
        [ text "reset" ]
      ]
  ]

-------------------------------------------------------------------------------
-- canvas
-------------------------------------------------------------------------------

initCanvas :: DOMRef -> Canvas ()
initCanvas _ = pure ()

drawCanvas :: Model -> () -> Canvas ()
drawCanvas _ () = do
  -- clear
  clearRect (0, 0, canvasWidthD, canvasHeightD)

  -- draw background
  fillStyle (color $ Style.Hex "DDDDDD")
  fillRect (0, 0, canvasWidthD, canvasHeightD)

  -- draw grid
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


 
