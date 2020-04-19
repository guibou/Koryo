{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DisambiguateRecordFields #-}
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
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Player = Player
  {
    name :: String,
    board :: Map Card Int,
    nbCoins :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Hand
  = Draw (Map Card Int)
  | Selected SelectedFromDraw
  | DoActions Actions
  | NothingToDo
  | WaitingForDestroying
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Actions = Actions {
  flipAction :: Int, -- Action of the black -1. Must select
  kill :: Int, -- Action of the red -1. Must select.
  takeCoin :: Bool, -- Action of the 6. Can be automated
  stealCoin :: Bool, -- Action of the 2. Must select.
  destroyCard :: Bool -- Action of the 4. Can be automated
  }
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
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Phase = Drawing | Playing | Destroying
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data TopLevelGame = TopLevelGame
  {
    game :: Game,
    randomGenerator :: StdGen,
    handles :: [Hand]
  }
  deriving (Show, Generic)

nbCards :: Map Card Int -> Int
nbCards = sum . Map.elems

startGame :: StdGen -> TopLevelGame
startGame gen = TopLevelGame {
  game = Game {
      players = [],
      currentRound = 1,
      currentFirstPlayer = 0,
      availableCoins = 8,
      selectedPlayer = 0,
      phase = Drawing
      },
    handles = [],
    randomGenerator = gen
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

  cardsToDraw = map (\p -> cardsDrawAtRound (currentRound game) + bool 0 1 (Just p == playerWithMajorityFor8)) [0 .. length (players game) - 1]

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
-- idempotent
selectCard :: Int -> SelectedFromDraw -> TopLevelGame -> TopLevelGame
selectCard pNumber selected tgame = set (#handles . ix pNumber) (Selected selected) tgame
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


revealPlayer :: SelectedFromDraw -> Player -> (Player, Actions)
revealPlayer handle player = let
  newCards = selectionToMap handle
  actions = Actions {
    takeCoin = True,
    stealCoin = True,
    destroyCard = True,
    flipAction = fromMaybe 0 (Map.lookup Cm1_FlipTwo newCards),
    kill = fromMaybe 0 (Map.lookup Cm1_KillOne newCards)
    }


  in (player {
     board = Map.unionWith (+) (board player) newCards
     }, actions)

{-
   - Each player destroy card, if needed (power 3)
C) End of game

   - Count the points
-}

-- Idempotent
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
  currentFirstPlayer = (currentFirstPlayer game + 1) `mod` (length (players game)),
  selectedPlayer = (currentFirstPlayer game + 1) `mod` (length (players game)),
  currentRound = currentRound game + 1,
  phase = Drawing
  }

destroyingPhase :: TopLevelGame -> TopLevelGame
destroyingPhase tg = tg
  {
    game = (game tg) {
      selectedPlayer = currentFirstPlayer (game tg),
      phase = Destroying
   },
    handles = map (const WaitingForDestroying) (handles tg)
   }

-- idempotent
endPlayerTurn :: Int -> TopLevelGame -> TopLevelGame
endPlayerTurn pId tg
  -- Ensure that the current player is the right one
  | view (#game . #selectedPlayer) tg /= pId = tg
endPlayerTurn _pId (over #game nextPlayer . tellHimToDoNothing ->tg)
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
  | otherwise = destroyingPhase tg

areAllSelected :: [Hand] -> Maybe [SelectedFromDraw]
areAllSelected [] = Just []
areAllSelected (x:xs) = do
  ls <- areAllSelected xs

  case x of
    Selected l -> Just (l:ls)
    _ -> Nothing

takeCoinInTheBank :: TopLevelGame -> Int -> TopLevelGame
takeCoinInTheBank tg playerId
  -- Ensure player can take a coin
  | preview (#handles . ix playerId . #_DoActions . #takeCoin) tg == Just False = tg
  | availableCoins (game tg) == 0 = tg
  | otherwise = over (#game . #players . ix playerId . #nbCoins) (+1) $
                set (#handles . ix playerId . #_DoActions . #takeCoin) False $
                over (#game . #availableCoins) (subtract 1) tg

destroyAPersonalCard :: TopLevelGame -> Int -> TopLevelGame
destroyAPersonalCard tg playerId
  -- Ensure player can destroy its card
  | preview (#handles . ix playerId . #_DoActions . #destroyCard) tg == Just False = tg
  -- Destroy a KillOne
  | Just n <- Map.lookup Cm1_KillOne . board . (!! playerId) . players . game $ tg
  , n > 0 = set (#handles . ix playerId . #_DoActions . #destroyCard) False $
                over (#game . #players . ix playerId . #board) (Map.insert Cm1_KillOne (n - 1)) $ tg
  -- Destroy a FlipTwo
  | Just n <- Map.lookup Cm1_FlipTwo . board . (!! playerId) . players . game $ tg
  , n > 0 = set (#handles . ix playerId . #_DoActions . #destroyCard) False $
                over (#game . #players . ix playerId . #board) (Map.insert Cm1_FlipTwo (n - 1)) tg
  | otherwise = set (#handles . ix playerId . #_DoActions . #destroyCard) False tg

stealACoinToPlayer :: TopLevelGame -> Int -> Int -> TopLevelGame
stealACoinToPlayer tg currentPlayerId otherPlayerId
  -- Ensure that player can do this action
  | preview (#handles . ix currentPlayerId . #_DoActions . #stealCoin) tg == Just False = tg
  | (nbCoins . (!! otherPlayerId) . players . game) tg > 0 =
    over (#game . #players . ix currentPlayerId . #nbCoins) (+1) $
    over (#game . #players . ix otherPlayerId . #nbCoins) (subtract 1) $
    set (#handles . ix currentPlayerId . #_DoActions . #stealCoin) False $ tg
  | otherwise =
    set (#handles . ix currentPlayerId . #_DoActions . #stealCoin) False $ tg

-- TODO: do some check
fireCommand :: TopLevelGame -> Int -> (Int, Card) -> TopLevelGame
fireCommand tg currentPlayerId (targetId, card)
  -- Ensure we have the power
  | Just n <- preview (#handles . ix currentPlayerId . #_DoActions . #kill) tg
  , n > 0 =
    over (#game . #players . ix targetId . #board) (Map.insertWith (+) card (-1)) $
    over (#handles . ix currentPlayerId . #_DoActions . #kill) (subtract 1) $ tg
  | otherwise = tg

-- TODO: do some check
flipCommand :: TopLevelGame -> Int -> (Int, Card) -> (Int, Card) -> TopLevelGame
flipCommand tg currentPlayerId (targetId, card) (targetId', card')
  -- Ensure we have the power
  | Just n <- preview (#handles . ix currentPlayerId . #_DoActions . #flipAction) tg
  , n > 0 =
    over (#game . #players . ix targetId . #board) (Map.unionWith (+) (Map.fromList [(card, (-1)), (card', 1)])) $
    over (#game . #players . ix targetId' . #board) (Map.unionWith (+) (Map.fromList [(card', (-1)), (card, 1)])) $
    over (#handles . ix currentPlayerId . #_DoActions . #flipAction) (subtract 1) $ tg
  | otherwise = tg


dropCards :: TopLevelGame -> Int -> Map Card Int -> TopLevelGame
dropCards tg' pId cards
  | view (#game . #selectedPlayer) tg == view (#game . #currentFirstPlayer) tg = drawPhase (tg {
                                                                                            game = nextRound (game tg)
                                                                                            })

  | otherwise = tg
      where
        tg = dropCardsForPlayer tg' pId cards

-- idempotent
dropCardsForPlayer :: TopLevelGame -> Int -> Map Card Int -> TopLevelGame
dropCardsForPlayer tg pId droppedCards
  -- Ensure that it is the right player
  | pId == view (#game . #selectedPlayer) tg =
    -- go to next player
    over (#game . #selectedPlayer) (\i -> (i + 1) `mod` (length (view (#game . #players) tg))) $
    -- set current player to nothing to do
    set (#handles . ix pId) NothingToDo $
    -- remove his cards
    over (#game . #players . ix pId . #board) (flip (Map.unionWith (-)) droppedCards) $ tg
  | otherwise = tg

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
  = SelectHand SelectedFromDraw
  | EndTurn
  | TakeCoinCommand
  | DestroyCardCommand
  | StealACoinToPlayer Int
  | FireCommand (Int, Card)
  | FlipCommand (Int, Card) (Int, Card)
  | DropCards (Map Card Int)
  deriving (ToJSON, FromJSON, Generic, Show)

data RemoteCommand
  = Login String
  | GameCommand Int KoryoCommands
  deriving (ToJSON, FromJSON, Generic, Show)

data Payload = Payload Game Hand Int
  deriving (ToJSON, FromJSON, Generic, Show)


-- TODO: auto run
-- Play phase: do the 4 and 6 if possible. terminate the run automatically if there is no -1 to play.
-- Delete phase: next phase automatically if there is nothing to delete

-- Powers
-- OK. 1: see others
-- OK 2: (only possible with actions)
-- OK3:
-- OK. 4: (automatic during a RUN)
-- OK. 5 (with 1): handled in UI
-- OK 6. (automatic during a RUN?)
-- OK 7: (Only possible with actions)
-- OK. 8 (with 1): handled
-- OK. 9: handled (only for score)
-- OK: -1 black (will work with 2)
-- OK: -1 red (will work with 7)
