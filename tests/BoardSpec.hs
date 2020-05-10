{-# OPTIONS -Wno-orphans #-}
module BoardSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Monad (replicateM)
import Prelude hiding (lookup)
import Control.Lens (view, set)

import Test.QuickCheck.Instances.Natural ()

import Board
import Cards

-- * Generate

genCard :: Gen Card
genCard = elements [minBound..maxBound]

genHand :: Gen Board
genHand = do
  l <- replicateM 10 $ do
    c <- elements [minBound..maxBound]
    n <- arbitrary

    pure (c, n)
  pure $ Board.fromList l

instance Arbitrary Card where
  arbitrary = genCard

instance Arbitrary Board where
  arbitrary = genHand

-- * Spec

spec :: SpecWith ()
spec = do
  describe "nbCards" $ do
    it "works for all cards" $ do
      nbCards allCards `shouldBe` 55
    it "works with singleton" $ do
      property $ \c -> nbCards (singleton c) === 1
    it "0" $ do
      property $ \() -> nbCards mempty === 0

  describe "monoid" $ do
    it "preserve count" $ do
      property $ \a b -> nbCards a + nbCards b === nbCards (a <> b)
    it "preserve lookup" $ do
      property $ \a b c -> lookup c a + lookup c b === lookup c (a <> b)

  describe "unsafeDifference" $ do
    it "distribute" $ do
      property $ \a b -> (a <> b) `unsafeDifference` b === a

  describe "lens" $ do
    it "view" $ do
      property $ \a c -> lookup c a == view (cardAt c) a
    it "set" $ do
      property $ \a c v -> lookup c (set (cardAt c) v a) === v
