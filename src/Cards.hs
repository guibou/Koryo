{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Cards where

import GHC.Generics
import Data.Aeson

-- | All the card type from the game.
data Card
  = C1_GivePrio
  | C2_Ninja
  | C3_SaveTwoCards
  | C4_KillMinusOne
  | C5_TakeTwoDifferent
  | C6_Bank
  | C7_Warrior
  | C8_DrawTwoMore
  | C9_DoNothing
  | Cm1_KillOne
  | Cm1_FlipTwo
  deriving (Show, Bounded, Enum, Ord, Eq, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | Return the number of card in the game
cardCount :: Card -> Int
cardCount = \case
  C1_GivePrio -> 1
  C2_Ninja -> 2
  C3_SaveTwoCards -> 3
  C4_KillMinusOne -> 4
  C5_TakeTwoDifferent -> 5
  C6_Bank -> 6
  C7_Warrior -> 7
  C8_DrawTwoMore -> 8
  C9_DoNothing -> 9
  Cm1_KillOne -> 6
  Cm1_FlipTwo -> 4
