{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Update where

import Miso
import Miso.JSON (withArray, withObject, (.:))
import Miso.Lens

import Game
import Helpers
import Model

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

data Action
  = ActionAskReset Mode
  | ActionAskPlay (Int, (Double, Double))
  | ActionSetModel Model
  | ActionSetGame Game
  | ActionToggleFlagMode
  | ActionNoOp

-------------------------------------------------------------------------------
-- event decoding
-------------------------------------------------------------------------------

-- | Decode the pointer button plus the position scaled from CSS pixels into
-- the canvas' logical coordinates, so the board can shrink responsively.
playDecoder :: Decoder (Int, (Double, Double))
playDecoder = Decoder
  { decodeAt = DecodeTargets [ [], [ "target" ] ]
  , decoder = withArray "play" $ \case
      [ ev, tgt ] -> do
        (btn, ox, oy) <- flip (withObject "event") ev $ \o ->
          (,,) <$> o .: "button" <*> o .: "offsetX" <*> o .: "offsetY"
        (w, h, cw, ch) <- flip (withObject "target") tgt $ \o ->
          (,,,) <$> o .: "width" <*> o .: "height"
                <*> o .: "clientWidth" <*> o .: "clientHeight"
        pure (btn, (ox * w / max 1 cw, oy * h / max 1 ch))
      _ -> fail "expected [event, target]"
  }

-------------------------------------------------------------------------------
-- update
-------------------------------------------------------------------------------

updateModel :: Action -> Effect context props Model Action

updateModel (ActionAskReset mode) = do
  model <- get
  io (ActionSetModel <$> liftIO (resetModel mode model))

updateModel (ActionSetModel model) =
  put model

updateModel (ActionSetGame game) =
  mGame .= game

updateModel ActionToggleFlagMode =
  mFlagMode %= not

updateModel ActionNoOp =
  pure ()

updateModel (ActionAskPlay (btn, xy)) = do
  let (i, j) = uncurry xy2ij xy
  game <- use mGame
  flagging <- use mFlagMode
  let move
        | btn == 0 && not flagging = Just (MoveFree i j)
        | btn == 0 || btn == 1 || btn == 2 = Just (MoveFlag i j)
        | otherwise = Nothing
  case move of
    Nothing -> pure ()
    Just m  -> io (ActionSetGame <$> liftIO (play m game))
