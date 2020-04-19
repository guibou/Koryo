{-# LANGUAGE OverloadedStrings #-}
module UIReflex.CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay
import Koryo
import Assets
import Data.String
import Data.ByteString (ByteString)
import Data.Text (Text)


css :: Text -> ByteString
css hostname = (encodeUtf8 . toStrict . render) $ do
  body ? do
    backgroundColor indigo
    color white

  "td" ? do
    borderColor green
    borderWidth (px 30)
    borderStyle solid

  ".gameStatus" ? do
    display flex
    "p" ? do
      borderColor black
      borderWidth (px 1)
      borderStyle solid

  ".handle" ? do
    mempty

  ".gameArea" ? do
    display flex

  ".players" ? do
    mempty

  ".firstPlayer" ? do
    color gold

  ".currentPlayer" ? do
    color red

  ".players" |> "div:first-child" ? do
    marginBottom (vh 5)
    borderColor green
    borderWidth (px 5)
    borderStyle solid

  ".players" |> div ? do
    borderColor black
    borderWidth (px 1)
    borderStyle solid

  ".cards" ? do
    display flex

  ".card.selected" ? do
    outlineWidth (px 5)
    outlineStyle solid
    zIndex 1

  ".card[data-count~=\"0\"]" ? do
    ".help" ? do
      opacity 0
    width (px 0)

    -- So we cannot click on it when disapearing
    pointerEvents none

  ".selecting-flip" ? ".card.selected" ? do
    outlineColor black

  ".selecting-fire" ? ".card.selected" ? do
    outlineColor red

  ".card" ? do
    ".help" ? do
      opacity 100
    position relative

    backgroundSize contain
    width (vw 6)
    height (vw (6 / 0.62))

  ".card" ? do
    "transition" -: "width 1s"
    backgroundRepeat noRepeat

  ".card" ? ".burger" ? do
    color black
    pointerEvents none
    position absolute
    bottom (px 0)
    left (px 0)

    "div.visible" ? do
      visibility visible
      animation "appear" (sec 0.5) linear (sec 0) (iterationCount 1) alternate forwards

    "div.visible.lt" ? span ? do
      opacity 0

    "div.visible.eq" ? span ? do
      visibility visible
      zIndex 4

    "div" ? do
      "transition" -: "opacity 1s"
      animation "disappear" (sec 1) linear (sec 0) (iterationCount 1) alternate forwards

    "div.visible.eq" ? do
      animation "appear" (sec 1) linear (sec 0) (iterationCount 1) alternate forwards
      zIndex 3

    "div" ? span ? do
      mempty
      -- visibility hidden

    "div" ? do
      -- opacity 0
      pointerEvents none
      backgroundColor gold
      borderColor gold
      borderWidth (px 3)
      borderRadius (px 5) (px 5) (px 5) (px 5)
      padding (px 0) (px 5) (px 0) (px 5)
      margin (px 2) (px 0) (px 2) (px 0)

  ".blinking" ? "button" ? do
    animation "blinkingText" (sec 1) linear (sec 0) infinite alternate forwards

  keyframes "blinkingText" [
    (0, color black),
    (100, color red)
    ]

  keyframes "appear" [
    (0, opacity 0 <> transform (scale 0 0)),
    (50, opacity 100 <> transform (scale 3 3)),
    (100, opacity 100)
    ]

  keyframes "disappear" [
    (0, transform (scale 1 1)),
    (50, transform (scale 3 3) <> backgroundColor red),
    (100, transform (scale 0 0))
    ]

  ".help" ? do
    ".marker" ? do
      backgroundColor purple
      borderColor darkviolet
      color white
      position absolute
      bottom (px 0)
      right (px 0)
      borderWidth (px 3)
      borderRadius (px 5) (px 5) (px 5) (px 5)
      padding (px 0) (px 5) (px 0) (px 5)
      margin (px 2) (px 0) (px 2) (px 0)
    ".description" ? do
      opacity 0
      position absolute
      bottom (px 0)
      right (px 0)
      pointerEvents none

  ".help:active" ? do
    ".description" ? do
      userSelect none
      opacity 100

      zIndex 2
      top (px 0)
      right (px 0)
      borderWidth (px 3)
      borderRadius (px 5) (px 5) (px 5) (px 5)
      padding (px 0) (px 5) (px 0) (px 5)
      margin (px 2) (px 0) (px 2) (px 0)
      backgroundColor purple
      color white

  flip mapM_ enumerated $ \card -> do
    ".cards" ? (fromString $ "." <> show card) ? do
      backgroundImage (url $ card_url hostname card)

  ".roundedBlock" ? do
    borderWidth (px 4)
    borderColor purple
    borderStyle solid
    backgroundColor orchid

  ".coins" ? do
    display flex
    "div" ? do
      animation "disappear" (sec 1) linear (sec 0) (iterationCount 1) alternate forwards
      opacity 0
      backgroundImage (url $ coin_url hostname)
      backgroundSize contain
      backgroundRepeat noRepeat
      width (vw 4)
      "span" ? do
        zIndex (-1)

    "div.visible" ? do
      opacity 100
      animation "appear" (sec 0.5) linear (sec 0) (iterationCount 1) alternate forwards
