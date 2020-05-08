{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- This module introduce a board, or hand, full of 'Card'.
module Board (
  allCards,
  nbCards,
  lookup,
  fromList,
  difference,
  singleton,
  weightedPickMap,
  toList,
  cardAt,
  Board
 ) where

import Data.Aeson
import GHC.Generics
import System.Random.Shuffle
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import System.Random
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Merge
import Prelude hiding (lookup)
import Control.Lens

import Cards

-- | Board game.
newtype Board = Board (Map Card Int)
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

-- Note: the 'Eq' and Json instances are built so 0 does not appear in the internal representation.

-- | All the 'Card' of the game.
allCards :: Board
allCards = fromList (map (\c -> (c, cardCount c)) [minBound..maxBound])

-- | Number of 'Card' in a 'Board'.
nbCards :: Board -> Int
nbCards (Board m) = sum (Map.elems m)

-- | Number of similar 'Card' in this 'Board'.
lookup :: Card -> Board -> Int
lookup c (Board m) = fromMaybe 0 (Map.lookup c m)

filterZero :: Map k Int -> Map k Int
filterZero = Map.filter (/=0)

-- | Build a version without 0
mkBoard :: Map Card Int -> Board
mkBoard = Board . filterZero

-- | Returns a Board from a list of pair.
fromList :: [(Card, Int)] -> Board
fromList l = mkBoard (Map.fromListWith (+) l)

-- | @difference a b@ returns the 'Board' @a@ without cards of 'Board' @b@.
-- This functions is *partial* and raises an exception if you try to
-- remove cards which are not in the initial map.
difference :: Board -> Board -> Board
difference (Board a) (Board b) = mkBoard $ Merge.merge Merge.preserveMissing (Merge.mapMaybeMissing $ \k x -> error ("Merging incompatible Maps" <> show (k, x))) (Merge.zipWithMatched (\_k -> (-))) a b

-- | A board with only one card.
singleton :: Card -> Board
singleton c = Board (Map.singleton c 1)

-- | Convert to a list of pair. Only non null values will appears in the list.
toList :: Board -> [(Card, Int)]
toList (Board b) = Map.toList (filterZero b)

-- | Sum two 'Board'.
instance Semigroup Board where
  (<>) (Board a) (Board b) = Board (Map.unionWith (+) a b)

-- | Empty 'Board'.
instance Monoid Board where
  mempty = Board Map.empty

-- | Randomly pick n cards.
weightedPickMap ::
  -- | Initial board
  Board ->
  -- | Number of card to pick
  Int ->
  StdGen ->
  -- | The resulting board.
  (Board, StdGen)
weightedPickMap (Board currentMap) n gen =
  let
    listOfValues = concatMap (\(c, count) -> replicate count c) $ Map.toList currentMap
    (genA, genB) = split gen
    shuffled = take n $ shuffle' listOfValues (length listOfValues) genA

  in (foldMap singleton shuffled, genB)

-- * Some lens

-- | Lens to the number of 'Card' in a 'Board'.
cardAt :: Functor f => Card -> (Int -> f Int) -> Board -> f Board
cardAt c = lens (lookup c) setter
  where
    setter (Board m) i' = Board (Map.insert c i' m)
