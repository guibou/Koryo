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
import Clay.Stylesheet
import Clay.Media (screen)

css :: Text -> ByteString
css hostname = (encodeUtf8 . toStrict . render) $ do
  query screen [Feature "orientation" (Just "landscape")] $ do
    ".players" ? do
      display grid
      "grid-template-columns" -: "1fr 1fr"
      "grid-gap" -: "2vh"

  query screen [Feature "orientation" (Just "portrait")] $ do
    ".players" ? do
      display grid
      "grid-template-columns" -: "1fr"
      "grid-gap" -: "2vh"


  button ? do
    fontSize (em 2)

  body ? do
    backgroundColor lightpink
    color black
    fontSize (em 1.5)

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

  ".firstPlayer" ? do
    color gold

  ".currentPlayer" ? do
    color red

  ".players" |> "div:nth-child(1)" ? do
    borderColor deeppink
    borderWidth (px 5)
    borderStyle solid

  ".players" |> div ? do
    borderColor black
    borderWidth (px 1)
    borderStyle solid
    backgroundColor darkmagenta

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
    animation "tint-black" (sec 0.5) linear (sec 0) infinite alternate forwards

  ".selecting-fire" ? ".card.selected" ? do
    outlineColor red
    animation "tint-red" (sec 0.5) linear (sec 0) infinite alternate forwards

  ".card" ? do
    ".help" ? do
      opacity 100
    position relative

    let
      cardSize = 15
      cardRatio = 0.81

    backgroundSize contain
    width (vw cardSize)
    height (vw (cardSize / cardRatio))
    maxWidth (cardRatio *@ (vh cardSize))
    maxHeight  (vh cardSize)

  ".card" ? do
    -- "transition" -: "width 1s"
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

  keyframes "tint-red" [
    (0, "box-shadow" -: "0 3000px rgba(255, 0, 0, 0.8) inset"),
    (100, "box-shadow" -: "0 3000px rgba(255, 0, 0, 0) inset")
    ]

  keyframes "tint-black" [
    (0, "box-shadow" -: "0 3000px rgba(0, 0, 0, 0.8) inset"),
    (100, "box-shadow" -: "0 3000px rgba(0, 0, 0, 0) inset")
    ]
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
    borderColor darkcyan
    borderStyle solid
    backgroundColor aquamarine

  ".destroyor" ? do
    borderWidth (px 4)
    borderColor coral
    borderStyle solid
    backgroundColor crimson

  ".coins.canSteal" ? ":first-child" ? do
      animation "blinkCoin" (sec 1) linear (sec 0) infinite alternate forwards


  ".gameStatus" ? ".coins" ? do
    marginLeft auto

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
      width (vw 5)
      height (vw 5)
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
    fontSize (em 1.3)
    color white


  ".devMode" ? do
    display grid
    "grid-template-columns" -: "1fr 1fr"
