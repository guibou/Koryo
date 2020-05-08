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
import System.Random
import Data.List (find)
import Data.Maybe (fromMaybe,catMaybes,fromJust)
import Test.Hspec
import Data.Bool (bool)
import Control.Lens (view, set, over, ix, preview, (^?))
import Data.Aeson
import GHC.Generics
import Data.Generics.Labels()

import Cards
import Board (Board)
import qualified Board

data SelectedFromDraw
  = SelectMany Card Int
  | SelectTwo Card Card
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Player = Player
  {
    name :: String,
    board :: Board,
    nbCoins :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

newtype PlayerId = PlayerId Int
  deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Hand
  = Draw Board
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
newPlayer playerName = Player {name = playerName, board = mempty, nbCoins = 0}

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

hasTheOne :: Board -> Bool
hasTheOne cards = Board.lookup C1_GivePrio cards == 1

evaluateMajorityFor :: Card -> [Board] -> Maybe Int
evaluateMajorityFor card decks = let
  playerWithOne = fst <$> find (hasTheOne . snd) (zip [0..] decks)
  cardsCountPerPlayer = map (Board.lookup card) decks

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

-- TODO: corriger le compte du 1 pour le score
computeScores :: [Player] -> [Int]
computeScores players = let
  majoritiesFor = map (\c -> evaluateMajorityFor c (map (set (Board.cardAt C1_GivePrio) 0) . map board $ players)) [C1_GivePrio .. C9_DoNothing]
  penalities = map countPenalities players
  coins = map nbCoins players

  bonusFor1 = map (Board.lookup C1_GivePrio) (map board players)
  scores = Map.fromListWith (+) $ catMaybes $ zipWith (\playerM points -> (,points) <$> playerM)  majoritiesFor [1..9]

  in zipWith (+) bonusFor1 $ zipWith (+) (map (\pIdx -> fromMaybe 0 (Map.lookup pIdx scores)) [0..length players-1]) $ zipWith (+) coins penalities

countPenalities :: Player -> Int
countPenalities player = - (Board.lookup Cm1_KillOne (board player)) + Board.lookup Cm1_FlipTwo (board player)

{-
i) Drawing phase

   - First the game randomises card for each players (power 8)
-}

availableCards :: Game -> Board
availableCards game = Board.allCards `Board.difference ` (foldMap board (players game))

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
                   -> (StdGen, [Board], Board)
                   -> (StdGen, [Board], Board)
drawCards count (gen, acc, availablesCards) =
  let
    (draw, gen') = Board.weightedPickMap availablesCards count gen
  in (gen', draw:acc, availablesCards `Board.difference` draw)


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


selectionToMap :: SelectedFromDraw -> Board
selectionToMap sel = case sel of
  SelectMany c i -> foldMap Board.singleton (replicate i c)
  SelectTwo c c' -> Board.singleton c <> Board.singleton c'


revealPlayer :: SelectedFromDraw -> Player -> (Player, Actions)
revealPlayer handle player = let
  newCards = selectionToMap handle
  actions = Actions {
    takeCoin = True,
    stealCoin = True,
    destroyCard = True,
    flipAction = Board.lookup Cm1_FlipTwo newCards,
    kill = Board.lookup Cm1_KillOne newCards
    }


  in (player {
     board = board player <> newCards
     }, actions)

{-
   - Each player destroy card, if needed (power 3)
C) End of game

   - Count the points
-}

-- Idempotent
attemptRevealPhase :: HasCallStack => TopLevelGame -> TopLevelGame
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
endPlayerTurn :: HasCallStack => Int -> TopLevelGame -> TopLevelGame
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
  | let n = Board.lookup Cm1_KillOne . board . (!! playerId) . players . game $ tg
  , n > 0 = set (#handles . ix playerId . #_DoActions . #destroyCard) False $
                over (#game . #players . ix playerId . #board) (`Board.difference` Board.singleton Cm1_KillOne) $ tg
  -- Destroy a FlipTwo
  | let n = Board.lookup Cm1_FlipTwo . board . (!! playerId) . players . game $ tg
  , n > 0 = set (#handles . ix playerId . #_DoActions . #destroyCard) False $
                over (#game . #players . ix playerId . #board) (`Board.difference` Board.singleton Cm1_FlipTwo) tg
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
    over (#game . #players . ix targetId . #board) (`Board.difference` Board.singleton card) $
    over (#handles . ix currentPlayerId . #_DoActions . #kill) (subtract 1) $ tg
  | otherwise = tg

-- TODO: do some check
flipCommand :: TopLevelGame -> Int -> (Int, Card) -> (Int, Card) -> TopLevelGame
flipCommand tg currentPlayerId (targetId, card) (targetId', card')
  -- Ensure we have the power
  | Just n <- preview (#handles . ix currentPlayerId . #_DoActions . #flipAction) tg
  , n > 0 =
    over (#game . #players . ix targetId . #board) (\b -> b <> Board.singleton card' `Board.difference` Board.singleton card) $
    over (#game . #players . ix targetId' . #board) (\b -> b <> Board.singleton card `Board.difference` Board.singleton card') $
    over (#handles . ix currentPlayerId . #_DoActions . #flipAction) (subtract 1) $ tg
  | otherwise = tg


dropCards :: TopLevelGame -> Int -> Board -> TopLevelGame
dropCards tg' pId cards
  | view (#game . #selectedPlayer) tg == view (#game . #currentFirstPlayer) tg = drawPhase (tg {
                                                                                            game = nextRound (game tg)
                                                                                            })

  | otherwise = tg
      where
        tg = dropCardsForPlayer tg' pId cards

-- idempotent
dropCardsForPlayer :: TopLevelGame -> Int -> Board -> TopLevelGame
dropCardsForPlayer tg pId droppedCards
  -- Ensure that it is the right player
  | pId == view (#game . #selectedPlayer) tg
  && preview (#handles . ix pId) tg == Just WaitingForDestroying =
    -- go to next player
    over (#game . #selectedPlayer) (\i -> (i + 1) `mod` (length (view (#game . #players) tg))) $
    -- set current player to nothing to do
    set (#handles . ix pId) NothingToDo $
    -- remove his cards
    over (#game . #players . ix pId . #board) (flip Board.difference droppedCards) $ tg
  | otherwise = tg


-- Commands

data KoryoCommands
  = SelectHand SelectedFromDraw
  | EndTurn
  | TakeCoinCommand
  | DestroyCardCommand
  | StealACoinToPlayer Int
  | FireCommand (Int, Card)
  | FlipCommand (Int, Card) (Int, Card)
  | DropCards Board
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
