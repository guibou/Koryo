{-# LANGUAGE TemplateHaskell #-}

module Assets where

import Data.FileEmbed
import Data.ByteString.Base64
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Koryo

card_images :: Map Card ByteString
card_images = encode <$> Map.fromList [
  (C1_GivePrio, $(embedFile "images/1.png")),
  (C2_Ninja, $(embedFile "images/2.png")),
  (C3_SaveTwoCards, $(embedFile "images/3.png")),
  (C4_KillMinusOne, $(embedFile "images/4.png")),
  (C5_TakeTwoDifferent, $(embedFile "images/5.png")),
  (C6_Bank, $(embedFile "images/6.png")),
  (C7_Warrior, $(embedFile "images/7.png")),
  (C8_DrawTwoMore, $(embedFile "images/8.png")),
  (C9_DoNothing, $(embedFile "images/9.png")),
  (Cm1_KillOne, $(embedFile "images/barbare.png")),
  (Cm1_FlipTwo, $(embedFile "images/echange.png"))
  ]
