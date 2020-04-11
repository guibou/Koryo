{-# LANGUAGE ViewPatterns #-}
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
import Data.Maybe (fromMaybe,catMaybes,fromJust)
import Test.Hspec
import Data.Bool (bool)
import Control.Lens
import Data.Aeson
import GHC.Generics
import Data.Generics.Labels()
import Data.List (delete)

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
  | DoActions [Action]
  | NothingToDo
  deriving (Show, Generic, ToJSON, FromJSON)

-- TODO: refactor that to a single type, such as:
data Action
  = Flip Int -- Action of the black -1. Must select
  | Kill Int -- Action of the red -1. Must select.
  | TakeCoin -- Action of the 6. Can be automated
  | StealCoin -- Action of the 2. Must select.
  | DestroyCard -- Action of the 4. Can be automated
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Game = Game
  {
    players :: [Player],
    currentRound :: Int,
    availableCoins :: Int,
    currentFirstPlayer :: Int,
    selectedPlayer :: Int,
    phase :: Phase
   }
  deriving (Show, Generic, ToJSON, FromJSON)

data Phase = Drawing | Playing | Destroying
  deriving (Show, Generic, ToJSON, FromJSON)

data TopLevelGame = TopLevelGame
  {
    game :: Game,
    randomGenerator :: StdGen,
    handles :: [Hand]
  }
  deriving (Show, Generic)

startGame :: Int -> TopLevelGame
startGame seed = TopLevelGame {
  game = Game {
      players = [],
      currentRound = 1,
      currentFirstPlayer = 0,
      availableCoins = 8,
      selectedPlayer = 0,
      phase = Drawing
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


revealPlayer :: SelectedFromDraw -> Player -> (Player, [Action])
revealPlayer handle player = let
  newCards = selectionToMap handle
  actions = [TakeCoin, StealCoin, DestroyCard]
    <> catMaybes [
    Flip <$> Map.lookup Cm1_FlipTwo newCards,
    Kill <$> Map.lookup Cm1_KillOne newCards
    ]


  in (player {
     board = Map.unionWith (+) (board player) newCards
     }, actions)

-- TODO: the powers! (And user interaction!)

{-
   - Each player destroy card, if needed (power 3)
C) End of game

   - Count the points
-}

attemptRevealPhase :: TopLevelGame -> TopLevelGame
attemptRevealPhase tg@TopLevelGame{game, handles} = case areAllSelected handles of
  Nothing -> tg
  Just newCards -> let
    (newPlayer', actions) = revealPlayer (fromJust $ newCards ^? ix (currentFirstPlayer game)) ((players game) !! (currentFirstPlayer game))
    newGame = tg {
      game = game {
          selectedPlayer = currentFirstPlayer game,
          players = set (ix (currentFirstPlayer game)) newPlayer' (players game),
          phase = Playing
          },
      handles = set (ix (currentFirstPlayer game)) (DoActions actions) handles
      }
    in newGame

nextPlayer :: Game -> Game
nextPlayer game = game {
  selectedPlayer = (selectedPlayer game + 1) `mod` (length (players game))
  }

tellHimToDoNothing :: TopLevelGame -> TopLevelGame
tellHimToDoNothing tg = tg {
  handles = set (ix (selectedPlayer (game tg))) NothingToDo (handles tg)
  }

nextRound :: Game -> Game
nextRound game = game {
  currentFirstPlayer = currentFirstPlayer game + 1 `mod` (length (players game)),
  selectedPlayer = currentFirstPlayer game + 1 `mod` (length (players game)),
  currentRound = currentRound game + 1,
  phase = Drawing
  }

endPlayerTurn :: TopLevelGame -> TopLevelGame
endPlayerTurn (over #game nextPlayer . tellHimToDoNothing ->tg)
  | selectedPlayer (game tg) /= currentFirstPlayer (game tg) =
    let
      newCards = (\(Selected l) -> l) (handles tg !! (selectedPlayer (game tg)))
      (newPlayer', actions) = revealPlayer newCards (players (game tg) !! selectedPlayer (game tg))
      newGame = tg {
        game = (game tg) {
            players = set (ix (selectedPlayer (game tg))) newPlayer' (players (game tg))
            },
        handles = set (ix (selectedPlayer (game tg))) (DoActions actions) (handles tg)
        }
    in newGame
  | otherwise = drawPhase (tg {
                              game = nextRound (game tg)
                              })


areAllSelected :: [Hand] -> Maybe [SelectedFromDraw]
areAllSelected [] = Just []
areAllSelected (x:xs) = do
  ls <- areAllSelected xs

  case x of
    Selected l -> Just (l:ls)
    _ -> Nothing

takeCoinInTheBank :: TopLevelGame -> Int -> TopLevelGame
takeCoinInTheBank tg playerId
  | availableCoins (game tg) == 0 = tg
  | otherwise = over (#game . #players . ix playerId . #nbCoins) (+1) $
                over (#handles . ix playerId) (\(DoActions l) -> DoActions (delete TakeCoin l)) $
                over (#game . #availableCoins) (subtract 1) tg


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
  | EndTurn
  | TakeCoinCommand
  | DestroyCardCommand
  deriving (ToJSON, FromJSON, Generic, Show)

data Payload = Payload Game Hand Int
  deriving (ToJSON, FromJSON, Generic, Show)


-- Powers
-- OK. 1: see others
-- 2: (only possible with actions)
-- 3:
-- 4: (automatic during a RUN)
-- OK. 5 (with 1): handled in UI
-- OK 6. (automatic during a RUN?)
-- OK 7: (Only possible with actions)
-- OK. 8 (with 1): handled
-- OK. 9: handled (only for score)
-- -1 black (will work with 2)
-- -1 red (will work with 7)
