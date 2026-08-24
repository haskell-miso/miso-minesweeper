{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Miso.String (MisoString)
import Miso.CSS qualified as CSS

cellSize :: Int
cellSize = 32

cellSizeD :: Double
cellSizeD = fromIntegral cellSize

cellFont :: MisoString
cellFont = "700 17px ui-sans-serif, system-ui, sans-serif"

-- | Board / cell palette (modern flat look).
colorBoard, colorHidden1, colorHidden2, colorHiddenEdge,
  colorOpen, colorOpenEdge, colorWrongFlag, colorWrongMine :: CSS.Color
colorBoard      = CSS.Hex "1F2430" -- board background / grout
colorHidden1    = CSS.Hex "5B6B8C" -- unrevealed gradient, top
colorHidden2    = CSS.Hex "46536E" -- unrevealed gradient, bottom
colorHiddenEdge = CSS.Hex "6E7FA3" -- unrevealed highlight edge
colorOpen       = CSS.Hex "E8E4D8" -- revealed cell
colorOpenEdge   = CSS.Hex "D5D0C0" -- revealed cell border
colorWrongFlag  = CSS.Hex "E8B04B" -- flag that was wrong (game over)
colorWrongMine  = CSS.Hex "E4574F" -- the mine that was clicked

-- | Classic minesweeper number colors, tuned for the light cell background.
n2color :: Int -> CSS.Color
n2color = \case
  1 -> CSS.Hex "2563EB"
  2 -> CSS.Hex "15803D"
  3 -> CSS.Hex "DC2626"
  4 -> CSS.Hex "6D28D9"
  5 -> CSS.Hex "9A3412"
  6 -> CSS.Hex "0D9488"
  7 -> CSS.Hex "1F2937"
  _ -> CSS.Hex "4B5563"

ij2xy :: Int -> Int -> (Double, Double)
ij2xy i j = (fromIntegral (j * cellSize), fromIntegral (i * cellSize))

xy2ij :: Double -> Double -> (Int, Int)
xy2ij x y = (floor y `div` cellSize, floor x `div` cellSize)

data Mode
  = ModeBeginner
  | ModeIntermediate
  | ModeExpert
  deriving (Eq, Show)

mode2infos :: Mode -> (Int, Int, Int)
mode2infos = \case
  ModeBeginner      -> (9, 9, 10)
  ModeIntermediate  -> (16, 16, 40)
  ModeExpert        -> (16, 30, 99)
