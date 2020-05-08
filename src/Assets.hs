{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Assets where

import Data.Text (Text)
import Cards

card_path :: Card -> Text
card_path = \case
  C1_GivePrio -> "1.png"
  C2_Ninja -> "2.png"
  C3_SaveTwoCards -> "3.png"
  C4_KillMinusOne -> "4.png"
  C5_TakeTwoDifferent -> "5.png"
  C6_Bank -> "6.png"
  C7_Warrior -> "7.png"
  C8_DrawTwoMore -> "8.png"
  C9_DoNothing -> "9.png"
  Cm1_KillOne -> "rouge.png"
  Cm1_FlipTwo -> "noire.png"

card_url :: Text -> Card -> Text
card_url hostname c = "http://" <> hostname <> ":3004/" <> card_path c

-- Coin from https://en.wikipedia.org/wiki/Coin#/media/File:Claudius_II_coin_(colourised).png
coin_url hostname = "http://" <> hostname <> ":3004/coin.png"

-- backgroundImage (url $ "data:image/png;base64," <> card_images card)


{-
ImageMagick magic:

- trim the images
- remove the white background

Next time I'll tell my artist how to do that correctly initially.

for i in *.png; do echo $i; convert $i -fuzz 10% -fill none -draw "color 0,0 floodfill" -fuzz 20% -trim ${i}_2.png; done;

-}
