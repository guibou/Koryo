{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
module Koryo where

import qualified Data.Map as Map
import Data.Map (Map)
import System.Random
import System.Random.Shuffle
import Data.List (find)
import Data.Maybe (fromMaybe,catMaybes)
import Test.Hspec
import Data.Bool (bool)
import Control.Lens
import Data.Aeson
import GHC.Generics

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

enumerated :: (Bounded t, Enum t) => [t]
enumerated = [minBound..maxBound]

allCards :: Map Card Int
allCards = Map.fromList (zip enumerated ([1..9] ++ [6, 4]))

data SelectedFromDraw
  = SelectMany Card Int
  | SelectTwo Card Card
  deriving (Show, Generic, ToJSON, FromJSON)

data Player = Player
  {
    name :: String,
    board :: Map Card Int,
    nbCoins :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Hand
  = Draw (Map Card Int)
  | Selected SelectedFromDraw
  | DoActions [Card]
  | NothingToDo
  deriving (Show, Generic, ToJSON, FromJSON)

data Game = Game
  {
    players :: [Player],
    currentRound :: Int,
    availableCoins :: Int,
    currentFirstPlayer :: Int,
    selectedPlayer :: Int
   }
  deriving (Show, Generic, ToJSON, FromJSON)

data TopLevelGame = TopLevelGame
  {
    game :: Game,
    randomGenerator :: StdGen,
    handles :: [Hand]
  }
  deriving (Show)

startGame :: Int -> TopLevelGame
startGame seed = TopLevelGame {
  game = Game {
      players = [],
      currentRound = 1,
      currentFirstPlayer = 0,
      availableCoins = 8,
      selectedPlayer = 0
      },
    handles = [],
    randomGenerator = mkStdGen seed
  }

{-

Games phases

A) Gathering players

During this phase, the game can only accepts new players.

-}

addPlayer :: String -> Game -> Game
addPlayer playerName game = game { players = (newPlayer playerName) : players game }

newPlayer :: String -> Player
newPlayer playerName = Player {name = playerName, board = Map.empty, nbCoins = 0}

{-
B) Playing

We are in a round N

(power 1 is always used)
-}

-- * Round things
cardsDrawAtRound :: Int -> Int
cardsDrawAtRound roundNumber = 11 - roundNumber

cardsOnBoardAtRound :: Int -> Int
cardsOnBoardAtRound roundNumber = roundNumber + 2

hasTheOne :: Map Card Int -> Bool
hasTheOne cards = fromMaybe 0 (Map.lookup C1_GivePrio cards) == 1

evaluateMajorityFor :: Card -> [Map Card Int] -> Maybe Int
evaluateMajorityFor card decks = let
  playerWithOne = fst <$> find (hasTheOne . snd) (zip [0..] decks)
  cardsCountPerPlayer = map (fromMaybe 0 . Map.lookup card) decks

  maximumCardCount = maximum cardsCountPerPlayer

  in if maximumCardCount == 0
     then Nothing
     else
       let
         playersWithMax = map fst $ filter (\(_, c) -> c == maximumCardCount) (zip [0..] cardsCountPerPlayer)
       in case playersWithMax of
         [x] -> Just x
         l -> case playerWithOne of
           Just pl
             | pl `elem` l -> Just pl
             | otherwise -> Nothing
           Nothing -> Nothing

computeScores :: [Player] -> [Int]
computeScores players = let
  majoritiesFor = map (\c -> evaluateMajorityFor c (map board players)) [C1_GivePrio .. C9_DoNothing]
  penalities = map countPenalities players
  coins = map nbCoins players

  scores = Map.fromListWith (+) $ catMaybes $ zipWith (\playerM points -> (,points) <$> playerM)  majoritiesFor [1..9]

  in zipWith (+) (map (\pIdx -> fromMaybe 0 (Map.lookup pIdx scores)) [0..length players-1]) $ zipWith (+) coins penalities

countPenalities :: Player -> Int
countPenalities player = - (fromMaybe 0 (Map.lookup Cm1_KillOne (board player)) + fromMaybe 0 (Map.lookup Cm1_FlipTwo (board player)))

{-
i) Drawing phase

   - First the game randomises card for each players (power 8)
-}

availableCards :: Game -> Map Card Int
availableCards game = Map.unionWith (-) allCards $ Map.unionsWith (+) (fmap board (players game))

drawPhase :: TopLevelGame -> TopLevelGame
drawPhase TopLevelGame{game, randomGenerator} = let
  playerBoards = map board (players game)
  playerWithMajorityFor8 = evaluateMajorityFor C8_DrawTwoMore playerBoards

  cardsToDraw = map (\p -> cardsDrawAtRound (currentRound game) + bool 0 2 (Just p == playerWithMajorityFor8)) [0 .. length (players game) - 1]

  (newGen, newHands, _newAvailableCards) = foldr drawCards (randomGenerator, [], availableCards game) cardsToDraw

  in TopLevelGame {
    randomGenerator = newGen,
    game = game,
    handles = map Draw newHands
    }

drawCards ::
                   Int
                   -> (StdGen, [Map Card Int], Map Card Int)
                   -> (StdGen, [Map Card Int], Map Card Int)
drawCards count (gen, acc, availablesCards) =
  let
    (draw, gen') = weightedPickMap availablesCards count gen
  in (gen', draw:acc, Map.unionWith (-) availablesCards draw)


{-
   - Each players select their cards (power 5)
-}


-- TODO: ensure no cheaters
selectCard :: Int -> SelectedFromDraw -> TopLevelGame -> TopLevelGame
selectCard pNumber selected tgame =
  tgame {
    handles = (handles tgame & ix pNumber .~ Selected selected)
    }
{-
ii) Playing phase

   - Each player, in turn
     - Put their card on the field
     - do some effects (power 2, 4, 6, -1s)
       - during attacks, watch for power 7 and 2
-}


selectionToMap :: SelectedFromDraw -> Map Card Int
selectionToMap sel = case sel of
  SelectMany c i -> Map.singleton c i
  SelectTwo c c' -> Map.fromList [(c, 1), (c', 1)]


revealPlayer :: SelectedFromDraw -> Player -> Player
revealPlayer handle player = let
  newCards = selectionToMap handle
  in player { board = Map.unionWith (+) (board player) newCards }

-- TODO: the powers! (And user interaction!)

{-
   - Each player destroy card, if needed (power 3)
C) End of game

   - Count the points
-}

hugeRevealPhase :: TopLevelGame -> TopLevelGame
hugeRevealPhase tg@TopLevelGame{game, handles, randomGenerator} = case areAllSelected handles of
  Nothing -> tg
  Just newCards -> let
    newGame = TopLevelGame {
      handles = [],
      randomGenerator = randomGenerator,
      game = game {
          players = zipWith revealPlayer newCards (players game)
          }
      }
    in
    drawPhase (newGame {
                  game = (Koryo.game newGame) {
                      currentRound = currentRound game + 1,
                      currentFirstPlayer = currentFirstPlayer game + 1
                      }
                  })

areAllSelected :: [Hand] -> Maybe [SelectedFromDraw]
areAllSelected [] = Just []
areAllSelected (x:xs) = do
  ls <- areAllSelected xs

  case x of
    Selected l -> Just (l:ls)
    _ -> Nothing

-- * Random sampling primitive

weightedPickMap :: Map Card Int -> Int -> StdGen -> (Map Card Int, StdGen)
weightedPickMap currentMap n gen =
  let
    listOfValues = concatMap (\(c, count) -> replicate count c) $ Map.toList currentMap
    (genA, genB) = split gen
    shuffled = take n $ shuffle' listOfValues (length listOfValues) genA

  in (Map.fromListWith (+) (map (,1) shuffled), genB)


-- * Tests
spec :: SpecWith ()
spec = describe "majority" $ do
  describe "no 1" $ do
    it "no majority for equality" $ do
     evaluateMajorityFor C5_TakeTwoDifferent [Map.fromList [(C5_TakeTwoDifferent, 2)], Map.fromList [(C5_TakeTwoDifferent, 2)]] `shouldBe` Nothing
    it "majority" $ do
     evaluateMajorityFor C5_TakeTwoDifferent [Map.fromList [(C5_TakeTwoDifferent, 2)], Map.fromList [(C5_TakeTwoDifferent, 3)]] `shouldBe` Just 1
     evaluateMajorityFor C5_TakeTwoDifferent [Map.fromList [(C5_TakeTwoDifferent, 2)], Map.fromList [(C5_TakeTwoDifferent, 1)]] `shouldBe` Just 0
  describe "with 1" $ do
    it "majority for bigger" $ do
     evaluateMajorityFor C5_TakeTwoDifferent [Map.fromList [(C5_TakeTwoDifferent, 2)], Map.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 1)]] `shouldBe` Just 0
    it "majority for equal" $ do
     evaluateMajorityFor C5_TakeTwoDifferent [Map.fromList [(C5_TakeTwoDifferent, 2)], Map.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 2)]] `shouldBe` Just 1
    it "majority for bigger" $ do
     evaluateMajorityFor C5_TakeTwoDifferent [Map.fromList [(C5_TakeTwoDifferent, 2)], Map.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 3)]] `shouldBe` Just 1
    it "no majority for zero" $ do
     evaluateMajorityFor C5_TakeTwoDifferent [Map.fromList [(C5_TakeTwoDifferent, 0)], Map.fromList [(C1_GivePrio, 1), (C5_TakeTwoDifferent, 0)]] `shouldBe` Nothing


-- Commands

data KoryoCommands
  = AddPlayer String
  | ChangePlayer Int
  | SelectHand SelectedFromDraw
  deriving (ToJSON, FromJSON, Generic, Show)

data Payload = Payload Game Hand
  deriving (ToJSON, FromJSON, Generic, Show)


-- Powers
-- 8 (with 1): handled
-- 9: handled
