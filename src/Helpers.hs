{-# LANGUAGE OverloadedStrings #-}

module Helpers where

import Miso.String (MisoString)
import Miso.Style qualified as Style

cellSize :: Int
cellSize = 25

cellFont :: MisoString
cellFont = "small-caps bold 18px arial"

colorNo, colorYes, colorWrongFlag, colorWrongMine :: Style.Color
colorNo = Style.Hex "BBBBBB"
colorYes = Style.Hex "DDDDDD"
colorWrongFlag = Style.Hex "88DD88"
colorWrongMine = Style.Hex "DD8888"

n2color :: Int -> Style.Color
n2color = \case
  1 -> Style.Hex "0000FF"
  2 -> Style.Hex "007B00"
  3 -> Style.Hex "FF0000"
  4 -> Style.Hex "00007B"
  5 -> Style.Hex "7B0000"
  _ -> Style.black

ij2xy :: Int -> Int -> (Double, Double)
ij2xy i j = (fromIntegral (j*cellSize), fromIntegral (i*cellSize))

xy2ij :: Double -> Double -> (Int, Int)
xy2ij x y = (floor y `div` cellSize, floor x `div` cellSize)

data Mode
  = ModeBeginner
  | ModeIntermediate
  | ModeExpert

mode2infos :: Mode -> (Int, Int, Int)
mode2infos = \case
  ModeBeginner      -> (9, 9, 10)
  ModeIntermediate  -> (16, 16, 40)
  ModeExpert        -> (16, 30, 99)

