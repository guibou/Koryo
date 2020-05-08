{-# LANGUAGE OverloadedStrings #-}
module UIReflex.CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay
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

  ".gameArea" ? do
    display flex
    -- flexDirection column
    "flex-wrap" -: "wrap"

  ".players" ? do
    display flex
    "flex-wrap" -: "wrap"
    mempty

  ".firstPlayer" ? do
    color gold

  ".currentPlayer" ? do
    color red

  ".players" |> "div:nth-child(2)" ? do
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
    "flex-wrap" -: "wrap"

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
    width (vw 12)
    height (vw (12 / 0.81))

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

  keyframes "blinkCoin" [
    (0, opacity 50),
    (100, opacity 100 <> ("filter" -: "sepia(90%) hue-rotate(180deg)") )
    ]

  keyframes "appear" [
    (0, opacity 0 <> transform (scale 0 0) <> display block),
    (50, opacity 100 <> transform (scale 3 3) <> display block),
    (100, opacity 100 <> display block)
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

  flip mapM_ [minBound..maxBound] $ \card -> do
    ".cards" ? (fromString $ "." <> show card) ? do
      backgroundImage (url $ card_url hostname card)

  ".roundedBlock" ? do
    borderWidth (px 4)
    borderColor purple
    borderStyle solid
    backgroundColor orchid

  ".coins.canSteal" ? ".last" ? do
      animation "blinkCoin" (sec 1) linear (sec 0) infinite alternate forwards

  ".coins" ? do
    display flex
    justifyContent center
    alignItems center
    "flex-wrap" -: "wrap"
    "div" ? do
      display none
      animation "disappear" (sec 1) linear (sec 0) (iterationCount 1) alternate forwards
      opacity 0
      transform (scale 0 0)
      backgroundImage (url $ coin_url hostname)
      backgroundSize contain
      backgroundRepeat noRepeat
      width (vw 4)
      "span" ? do
        visibility hidden

    "div.visible" ? do
      transform (scale 1 1)
      opacity 100
      display block
      animation "appear" (sec 0.5) linear (sec 0) (iterationCount 1) alternate forwards

{-
Tentative to disable animation on load

  ".preload" ? "*" ? do
    "transition" -: "none !important"
    "animation" -: "none !important"

-}

  ".gameWidget[data-current-round=\"9\"]" ? do
    ".handle" ? do
      display none
    ".endOfGame" ? do
      display block
    ".scores" ? do
      display block
    ".gameStatus" ? do
      display none

  ".endOfGame" ? do
    display none
  ".scores" ? do
    display none

  ".name" ? do
    display flex
