module KoryoSpec where

import Test.Hspec
import Test.QuickCheck

import Board
import Cards
import Koryo

import Test.QuickCheck.Instances.Natural ()

-- * Tests
spec :: SpecWith ()
spec = do
  describe "majority" $ do
    describe "no 1" $ do
      it "no majority for equality" $ do
       evaluateMajorityFor C5_TakeTwoDifferent [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C5_TakeTwoDifferent, 2)]] `shouldBe` Nothing
      it "majority" $ do
       evaluateMajorityFor C5_TakeTwoDifferent [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C5_TakeTwoDifferent, 3)]] `shouldBe` Just 1
       evaluateMajorityFor C5_TakeTwoDifferent [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C5_TakeTwoDifferent, 1)]] `shouldBe` Just 0
    describe "with 1" $ do
      it "majority for bigger" $ do
       evaluateMajorityFor C5_TakeTwoDifferent [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 1)]] `shouldBe` Just 0
      it "majority for equal" $ do
       evaluateMajorityFor C5_TakeTwoDifferent [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 2)]] `shouldBe` Just 1
      it "majority for bigger" $ do
       evaluateMajorityFor C5_TakeTwoDifferent [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 3)]] `shouldBe` Just 1
      it "no majority for zero" $ do
       evaluateMajorityFor C5_TakeTwoDifferent [Board.fromList [(C5_TakeTwoDifferent, 0)], Board.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 0)]] `shouldBe` Nothing

  describe "score" $ do
    let testScore bs = computeScores (map (\b -> Player {nbCoins = 0, board = b}) bs)

    describe "no 1" $ do
      it "no majority" $ do
        testScore [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C5_TakeTwoDifferent, 2)]] `shouldBe` [0, 0]
      it "majority" $ do
       testScore [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C5_TakeTwoDifferent, 3)]] `shouldBe` [0, 5]
       testScore [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C5_TakeTwoDifferent, 1)]] `shouldBe` [5, 0]
    describe "with 1" $ do
      it "majority for bigger" $ do
       testScore [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 1)]] `shouldBe` [5, 1]
      it "majority for equal" $ do
       testScore [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 2)]] `shouldBe` [0, 1]
      it "majority for bigger" $ do
       testScore [Board.fromList [(C5_TakeTwoDifferent, 2)], Board.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 3)]] `shouldBe` [0, 6]
      it "no majority for zero" $ do
       testScore [Board.fromList [(C5_TakeTwoDifferent, 0)], Board.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 0)]] `shouldBe` [0, 1]

    describe "penality" $ do
      it "fine" $ do
        property $ \x y -> testScore [Board.fromList [(Cm1_KillOne, x), (Cm1_FlipTwo, y)]] `shouldBe` [-(fromIntegral (x + y))]


{-
  describe "flip" $ do
    it "keep the number of card constant" $ do
      property (G
-}

  -- Tests for flip, ensure that there is no card creation.
  -- Roughly, I need a lot of property test
