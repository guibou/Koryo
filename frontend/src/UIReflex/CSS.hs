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

  ".players" |> div ? do
    borderColor black
    borderWidth (px 1)
    borderStyle solid

  ".currentPlayer" ? do
    borderWidth (px 1)
    borderStyle solid
    borderColor gold

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
