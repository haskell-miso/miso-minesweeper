{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module View where

import Control.Monad (forM_, when)
import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Canvas as Canvas
import Miso.Html.Element (a_, button_, code_, div_, footer_, h1_, header_, main_, p_, span_)
import Miso.Html.Event (onClick, onContextMenuWithOptions)
import Miso.Html.Property (class_, height_, href_, title_, width_)
import Miso.Lens
import Miso.CSS qualified as CSS

import Game
import Helpers
import Model
import Update

-------------------------------------------------------------------------------
-- view
-------------------------------------------------------------------------------

viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ model =
  div_
  [ class_ "app" ]
  [ header_
    [ class_ "hero" ]
    [ h1_ [] [ "🍜 💣 ", a_ [ href_ repoUrl ] [ "miso-minesweeper" ] ]
    , p_ [ class_ "tagline" ]
      [ "The classic, rendered on a 2D canvas from Haskell compiled to "
      , "WebAssembly. Left-click to sweep, right-click (or flag mode) to flag."
      ]
    , a_ [ class_ "gh", href_ repoUrl ] [ "View source on GitHub" ]
    ]
  , main_
    [ class_ "panel" ]
    [ div_
      [ class_ "modes" ]
      [ modeChip ModeBeginner "beginner"
      , modeChip ModeIntermediate "intermediate"
      , modeChip ModeExpert "expert"
      ]
    , div_
      [ class_ "hud" ]
      [ span_ [ class_ "stat" ]
        [ "🚩 ", text (ms (game ^. gFlags) <> " / " <> ms (game ^. gNbMines)) ]
      , button_
        [ class_ "face", title_ "restart"
        , onClick (ActionAskReset (model ^. mMode))
        ]
        [ text statusFace ]
      , button_
        [ class_ (if model ^. mFlagMode then "chip active" else "chip")
        , onClick ActionToggleFlagMode
        , title_ "toggle flag mode (for touch screens)"
        ]
        [ "🚩 flag mode" ]
      ]
    , div_
      [ class_ "board-wrap" ]
      [ Canvas.canvas
        [ width_ (ms (nj * cellSize))
        , height_ (ms (ni * cellSize))
        , class_ "board"
        , on "pointerup" playDecoder (\p _ _ -> ActionAskPlay p)
        , onContextMenuWithOptions preventDefault ActionNoOp
        ]
        (\_ -> pure ())
        (\() -> drawCanvas model)
      ]
    , p_
      [ class_ "status" ]
      [ text statusLine ]
    ]
  , footer_
    [ class_ "foot" ]
    [ p_ []
      [ "Built with "
      , a_ [ href_ "https://github.com/dmjio/miso" ] [ "miso" ]
      , ", a Haskell web framework — rendered with "
      , code_ [] [ "Miso.Canvas" ]
      , "."
      ]
    ]
  ]
  where
    repoUrl = "https://github.com/haskell-miso/miso-minesweeper"
    game = model ^. mGame
    (ni, nj) = game ^. gBoardNiNj
    statusFace = case game ^. gStatus of
      StatusRunning -> if model ^. mFlagMode then "🚩" else "🙂"
      StatusWon     -> "😎"
      StatusLost    -> "💥"
    statusLine = case game ^. gStatus of
      StatusRunning ->
        ms (game ^. gRemCells) <> " safe cells left"
      StatusWon  -> "You won! 🎉 Press the face to play again."
      StatusLost -> "Boom. Press the face to try again."
    modeChip mode label =
      button_
      [ class_ (if model ^. mMode == mode then "chip active" else "chip")
      , onClick (ActionAskReset mode)
      ]
      [ text (label :: MisoString) ]

-------------------------------------------------------------------------------
-- canvas
-------------------------------------------------------------------------------

drawCanvas :: Model -> Canvas ()
drawCanvas model = do
  let (ni, nj) = model ^. mGame ^. gBoardNiNj
      w = fromIntegral (nj * cellSize)
      h = fromIntegral (ni * cellSize)
  clearRect (0, 0, w, h)
  fillStyle (color colorBoard)
  fillRect (0, 0, w, h)
  font cellFont
  textAlign TextAlignCenter
  textBaseline TextBaselineMiddle
  forGame (model ^. mGame) drawGameCell

-------------------------------------------------------------------------------
-- drawing functions
-------------------------------------------------------------------------------

-- | Trace a rounded-rect path inset within the current cell.
cellPath :: Canvas ()
cellPath = do
  let m = 1.5           -- grout between cells
      r = 5             -- corner radius
      x0 = m
      y0 = m
      x1 = cellSizeD - m
      y1 = cellSizeD - m
  beginPath ()
  moveTo (x0 + r, y0)
  arcTo (x1, y0, x1, y1, r)
  arcTo (x1, y1, x0, y1, r)
  arcTo (x0, y1, x0, y0, r)
  arcTo (x0, y0, x1, y0, r)
  closePath ()

-- | An unrevealed cell: vertical gradient with a light top edge.
drawHidden :: Canvas ()
drawHidden = do
  g <- createLinearGradient (0, 0, 0, cellSizeD)
  addColorStop (0, colorHidden1) g
  addColorStop (1, colorHidden2) g
  fillStyle (gradient g)
  cellPath
  fill ()
  strokeStyle (color colorHiddenEdge)
  lineWidth 1
  beginPath ()
  moveTo (5, 2.5)
  lineTo (cellSizeD - 5, 2.5)
  stroke ()

-- | A revealed cell.
drawOpen :: Canvas ()
drawOpen = do
  fillStyle (color colorOpen)
  cellPath
  fill ()
  strokeStyle (color colorOpenEdge)
  lineWidth 1
  cellPath
  stroke ()

-- | A colored backdrop for end-of-game reveals (wrong flag / fatal mine).
drawAlert :: CSS.Color -> Canvas ()
drawAlert c = do
  fillStyle (color c)
  cellPath
  fill ()

drawMine :: Bool -> Int -> Int -> Canvas ()
drawMine fatal i j = do
  save ()
  translate (ij2xy i j)
  if fatal then drawAlert colorWrongMine else drawOpen
  let c = cellSizeD / 2
      spike = cellSizeD * 0.36
  -- spikes
  strokeStyle (color (CSS.Hex "1F2430"))
  lineWidth 2
  beginPath ()
  forM_ [ 0, 45, 90, 135 ] $ \deg -> do
    let a = deg * pi / 180
        dx = spike * cos a
        dy = spike * sin a
    moveTo (c - dx, c - dy)
    lineTo (c + dx, c + dy)
  stroke ()
  -- body with radial shading
  g <- createRadialGradient (c - 3, c - 3, 1, c, c, cellSizeD * 0.28)
  addColorStop (0, CSS.Hex "6B7280") g
  addColorStop (1, CSS.Hex "111827") g
  fillStyle (gradient g)
  beginPath ()
  arc (c, c, cellSizeD * 0.26, 0, 2 * pi)
  fill ()
  -- glint
  fillStyle (color (CSS.Hex "F9FAFB"))
  beginPath ()
  arc (c - cellSizeD * 0.09, c - cellSizeD * 0.09, cellSizeD * 0.05, 0, 2 * pi)
  fill ()
  restore ()

drawFlag :: Bool -> Int -> Int -> Canvas ()
drawFlag wrong i j = do
  save ()
  translate (ij2xy i j)
  if wrong then drawAlert colorWrongFlag else drawHidden
  let px = cellSizeD * 0.62 -- pole x
  -- pole
  strokeStyle (color (CSS.Hex "1F2430"))
  lineWidth 2
  beginPath ()
  moveTo (px, cellSizeD * 0.2)
  lineTo (px, cellSizeD * 0.8)
  stroke ()
  -- base mound
  fillStyle (color (CSS.Hex "1F2430"))
  beginPath ()
  moveTo (cellSizeD * 0.3, cellSizeD * 0.82)
  lineTo (cellSizeD * 0.85, cellSizeD * 0.82)
  lineTo (px, cellSizeD * 0.68)
  closePath ()
  fill ()
  -- waving flag
  fillStyle (color (CSS.Hex "EF4444"))
  beginPath ()
  moveTo (px, cellSizeD * 0.2)
  quadraticCurveTo (cellSizeD * 0.32, cellSizeD * 0.26, cellSizeD * 0.2, cellSizeD * 0.34)
  quadraticCurveTo (cellSizeD * 0.36, cellSizeD * 0.4, px, cellSizeD * 0.5)
  closePath ()
  fill ()
  restore ()

drawFree :: Int -> Int -> Int -> Canvas ()
drawFree i j n = do
  save ()
  translate (ij2xy i j)
  drawOpen
  when (n > 0) $ do
    fillStyle (color (n2color n))
    fillText (ms n, cellSizeD / 2, cellSizeD / 2 + 1)
  restore ()

drawGameCell :: Int -> Int -> Cell -> Canvas ()
drawGameCell i j = \case
  CellUnknown -> do
    save ()
    translate (ij2xy i j)
    drawHidden
    restore ()
  CellFree n  -> drawFree i j n
  CellFlag    -> drawFlag False i j
  CellFlagKo  -> drawFlag True i j
  CellMine    -> drawMine False i j
  CellMineKo  -> drawMine True i j
