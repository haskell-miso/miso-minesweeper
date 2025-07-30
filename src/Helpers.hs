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
-- xy2ij x y = (floor y `div` cellSize, floor x `div` cellSize)   -- TODO
xy2ij x y = (floor y' `div` cellSize, floor x' `div` cellSize)
  where
    x' = x - fromIntegral cellSize * 0.5
    y' = y - fromIntegral cellSize * 3.5

