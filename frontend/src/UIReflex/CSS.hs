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

  ".gameStatus" ?
    display flex

  ".players" ? do
    display flex

  ".players" |> div ? do
    borderColor black
    borderWidth (px 1)
    borderStyle solid

  ".cards" ? do
    display flex

  ".card" ? do
    position relative

    "img" ? do
      width (vw 9)

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
