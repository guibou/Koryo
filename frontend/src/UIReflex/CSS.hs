{-# LANGUAGE OverloadedStrings #-}
module UIReflex.CSS where

import Prelude hiding (div, span)

import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Clay

import Data.ByteString (ByteString)


css :: ByteString
css = (encodeUtf8 . toStrict . render) $ do
  body ? do
    backgroundColor white

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

  ".selecting-flip" ? ".card.selected" ? do
    outlineColor black

  ".selecting-fire" ? ".card.selected" ? do
    outlineColor red

  ".card" ? do
    position relative

    "img" ? do
      width (vw 6)

  ".card" ? do
    animation "appear" (sec 1) linear (sec 0) normal alternate forwards

  ".card" ? ".burger" ? do
    pointerEvents none
    position absolute
    bottom (px 0)
    left (px 0)

    "div:first-child" ? span ? do
      visibility visible

    "div" ? span ? do
      visibility hidden

    "div" ? do
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
    (0, opacity 0),
    (100, opacity 100)
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
