{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module UIReflex.UI (runUI) where

import UIReflex.CSS

import Reflex.Dom hiding (FireCommand)
import qualified Data.Text as Text
import Control.Monad (void)
import PyF (fmt)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text.Encoding
import Data.Traversable (for)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Generics.Labels()
import Control.Lens

import Koryo
import Assets
import qualified Data.Set as Set
import Data.Set (Set)

data GameState = NotPlaying
  deriving (Show)

listDyn :: MonadWidget t m => Dynamic t [a] -> m (Dynamic t [Dynamic t a])
listDyn input = do
  let
    onTheList l = map (\i -> (!!i) <$> input)  [0..(length l - 1)]
    newDyn = onTheList <$> input

    f a b = length a == length b

  holdUniqDynBy f newDyn

projectCardSelection pId cm = Set.fromList $ map snd $ filter ((==pId).fst) $ go cm
  where
    go NotSelecting = []
    go (Selecting (SelectingFire Nothing)) = []
    go (Selecting (SelectingFire (Just (pId', c)))) = [(pId', c)]
    go (Selecting (SelectingFlip Nothing)) = []
    go (Selecting (SelectingFlip (Just ((pId', c), m)))) = (pId', c):case m of
      Nothing -> []
      Just v -> [v]

-- TODO: there are many thing in this function which can be recovered from the Dynamic t Game
playerWidget :: MonadWidget t m => Dynamic t Game -> Dynamic t Int -> Dynamic t (Int -> Bool) -> Dynamic t CardSelectionMode -> (Int, Dynamic t Player) -> m (Event t (Either (Int, Card) KoryoCommands))
playerWidget game dCurrentPlayerId canStealDyn cardSelectDyn (playerNumber, player) = do
  (block, eStealCoin) <- el' "div" $ mdo
    let
      cls = ffor dCurrentPlayerId $ \currentPId ->
        if currentPId == playerNumber
        then "name currentPlayer"
        else "name"

    elDynClass "div" cls $ do
      dynText (Text.pack . name <$> player)
      dynText (ffor game $ \game -> if currentFirstPlayer game == playerNumber
                                    then " - First Player"
                                    else ""
              )
      dynText (ffor game $ \game -> if selectedPlayer game == playerNumber
                                    then " - Playing"
                                    else ""
              )
      dynText (ffor game $ \game ->
                  let currentScore = computeScores (players game) !! playerNumber
                  in [fmt| - Score: {currentScore}|])
    eStealCoin <- elClass "div" "coin" $ do
      text "Coins: "
      display (nbCoins <$> player)
      let enabled = ffor canStealDyn $ \canSteal -> bool ("disabled" =: "disabled") mempty (canSteal playerNumber)
      (b, _) <- elDynAttr' "button" enabled (text "Steal!")

      pure (StealACoinToPlayer playerNumber <$ domEvent Click b)

    eventSelectCard <- displayCards (projectCardSelection playerNumber <$> cardSelectDyn) (board <$> player)
    pure [Right <$> eStealCoin, Left . (playerNumber,) <$> eventSelectCard]

  pure $ leftmost eStealCoin

data CardSelectionMode
  = NotSelecting
  | Selecting SelectionMode
  deriving (Show)

data SelectionMode
  = SelectingFire (Maybe (Int, Card))
  | SelectingFlip (Maybe ((Int, Card), Maybe (Int, Card)))
  deriving (Show)

updateSelection :: (Int -> Attack -> Bool) -> (Int, Card) -> CardSelectionMode -> CardSelectionMode
updateSelection _ _ NotSelecting = NotSelecting
updateSelection _canBeFocused (_target, card) currentSel
  | card == Cm1_KillOne || card == Cm1_FlipTwo = currentSel -- You do not have the right to select a -1
updateSelection canBeFocused sel@(target, card) currentSel@(Selecting (SelectingFire b))
  | not (canBeFocused target FireAttack) = currentSel -- This player cannot be selected
  | otherwise = Selecting $ SelectingFire $ case b of
  Just sel'
    | sel == sel' -> Nothing -- Unselect same card
  _ -> Just sel -- Select (and replace)
updateSelection canBeFocused sel@(player, _) currentSel@(Selecting (SelectingFlip b))
  | not (canBeFocused player FlipAttack) = currentSel -- Do nothing, we cannot target this player
  | otherwise = Selecting $ SelectingFlip $ case b of
    Nothing -> Just (sel, Nothing) -- Select
    Just (sel'@(player', _), Nothing)
      | sel' == sel -> Nothing -- Unselect same card
      | player == player' -> Just (sel, Nothing) -- Replace selection on that player
      | otherwise -> Just (sel', Just sel) -- Add to selection
    Just (sel'@(player', _), Just sel''@(player'', _))
      | sel' == sel -> Just (sel'', Nothing) -- Unselect same card
      | sel'' == sel -> Just (sel', Nothing) -- Unselect same card
      | player' == player -> Just (sel'', Just sel) -- Replace selection on first player
      | player'' == player -> Just (sel', Just sel) -- Replace selection on second player
      | otherwise -> Just (sel'', Just sel) -- Select and discard the oldest

displayCards :: MonadWidget t m => Dynamic t (Set Card) -> Dynamic t (Map Card Int) -> m (Event t Card)
displayCards selectedCards cards = do
  selectCardEvent <- elClass "div" "cards" $ do
    listViewWithKey (Map.filter (/=0) <$> cards) $ \card dCount -> do
      let selected selStatus
            | card `Set.member` selStatus = "selected card"
            | otherwise = "card"
      elDynClass "div" (selected <$> selectedCards) $ do
        e <- case Map.lookup card card_images of
          Nothing -> text (Text.pack . show $ card) >> pure never
          Just t -> do
            (e, _) <- elAttr' "img" (Map.fromList [
                             ("src", "data:image/png;base64," <> (decodeUtf8 t))
                               --("width", "100")
                             ]) $ pure ()
            let cardClick = domEvent Click e
            pure $ cardClick
        widgetBurger dCount
        pure e

  pure $ ffor (Map.keys <$> selectCardEvent) $ \l -> case l of
    [c] -> c
    _ -> error "Too many card selected at once. That's impossible"

selectToCommand :: CardSelectionMode -> Maybe KoryoCommands
selectToCommand (Selecting (SelectingFlip (Just (a, Just b)))) = Just (FlipCommand a b)
selectToCommand (Selecting (SelectingFire (Just a))) = Just (FireCommand a)
selectToCommand _ = Nothing

displayHand :: forall t m. MonadWidget t m => Dynamic t Game -> Dynamic t Int -> Dynamic t CardSelectionMode -> (Card -> Bool) -> Hand -> m (Event t CardSelectionMode, Event t KoryoCommands)
displayHand _ _ _ _ NothingToDo = text "Wait until it is your turn" >> pure (never, never)
displayHand dGame dCurrentPlayerId _ _ WaitingForDestroying = do
  text "Destroying Phase"

  eDestroy <- dyn $ (handDestroyor <$> dGame <*> dCurrentPlayerId)

  eDestroyS <- switchHold never eDestroy

  pure (never, eDestroyS)

displayHand _ _ _ majoritySelector (Draw m) = do
  e <- handSelector (majoritySelector C5_TakeTwoDifferent) m
  pure (never, SelectHand <$> e)
displayHand _ _ _ _ (Selected sel) = do
  text "You selected some cards. Wait until it is your turn to play"
  void $ displayCards (constDyn mempty) (constDyn (selectionToMap sel))
  pure (never, never)
displayHand _ _ currentSelection majoritySelector (DoActions actions) = do
  let
    simpleText t = text t >> pure (never, never)
    btn t enabled event = do
      (b, _) <- elDynAttr' "button" (bool ("disabled" =: "disabled") mempty <$> enabled) $ text t

      pure (event <$ (domEvent Click b))

    selectCommand = selectToCommand <$> currentSelection

    -- TODO: check that some action can be done
  e <- mapM (el "div"$) $
    [
      do
        if flipAction actions /= 0
        then do
          e <- btn [fmt|Flip {flipAction actions}|] (constDyn True) (Selecting (SelectingFlip Nothing))
          runCommand <- btn "Confirm Flip" (ffor selectCommand $ \case
                               Just (FlipCommand _ _) -> True
                               _ -> False) ()
          pure (e, fmapMaybe id (current selectCommand <@ runCommand))
        else pure (never, never)
                 ,
      do
         if kill actions /= 0
           then do
             e <- btn [fmt|Kill {kill actions}|] (constDyn True) (Selecting (SelectingFire Nothing))
             runCommand <- btn "Confirm Fire" (ffor selectCommand $ \case
                                   Just (FireCommand _) -> True
                                   _ -> False) ()
             pure (e, fmapMaybe id (current selectCommand <@ runCommand))
           else pure (never, never)
                  ,
      -- TODO: test that there are available coins
      if majoritySelector C2_Ninja && stealCoin actions
           then simpleText "You can steal a coin to someone"
           else pure (never, never)
                  ,
      -- TODO: check that there is coin in the bank
      if takeCoin actions && majoritySelector C6_Bank
           then do
           e <- btn [fmt|Take a coin in the bank|] (constDyn True) TakeCoinCommand
           pure (never, e)
           else pure (never, never)
                  ,
      -- TODO: check that there is something to destroy
      if destroyCard actions && majoritySelector C4_KillMinusOne
           then do
             e <- btn "DestroyCard" (constDyn True) DestroyCardCommand
             pure (never, e)
           else pure (never, never)
                  ]

  let (selectionEvent, commandEvent) = unzip e

  (bEndTurn, _) <- el' "button" $ text "End Turn"

  pure $ (leftmost
          [
            -- End of turn clean the selection
            NotSelecting <$ domEvent Click bEndTurn,
            -- Any command clean the selection
            NotSelecting <$ leftmost commandEvent,
            leftmost selectionEvent
          ]
         ,
          leftmost [
             EndTurn <$ domEvent Click bEndTurn,
             leftmost commandEvent
             ]
         )

data Attack = FireAttack | FlipAttack
  deriving (Show)

canBeFocused :: [Map Card Int] -> Int -> Attack -> Bool
canBeFocused board pId FireAttack = evaluateMajorityFor C7_Warrior board /= Just pId
canBeFocused board pId FlipAttack = evaluateMajorityFor C2_Ninja board /= Just pId || fromMaybe 0 (Map.lookup C7_Warrior (board !! pId)) /= 0

widgetGame :: forall t m. MonadWidget t m => Dynamic t Payload -> m (Event t KoryoCommands)
widgetGame dPayload = mdo
  (events, selection) <- elDynClass "div" (ffor selection $ \case
                                              NotSelecting -> ""
                                              Selecting (SelectingFire _) -> "selecting-fire"
                                              Selecting (SelectingFlip _) -> "selecting-flip"
                                              ) $ mdo
    let dg = (\(Payload g _ _) -> g) <$> dPayload
    let dHand = (\(Payload _ h _) -> h) <$> dPayload
    let dCurrentPlayerId = (\(Payload _ _ i) -> i) <$> dPayload
    let
      canSteal game currentPlayerHand currentPlayerId otherPlayerId =
        -- enough coins for p1
        nbCoins (players (game) !! otherPlayerId) > 0
        -- Have the ninja for current player
          && evaluateMajorityFor C2_Ninja (map board (players game)) == Just currentPlayerId
          --  have a Steal command
          && case currentPlayerHand of
              DoActions l ->  stealCoin l
              _ -> False
          && currentPlayerId /= otherPlayerId

      canStealDyn = canSteal <$> dg <*> dHand <*> dCurrentPlayerId

    elClass "div" "gameStatus" $ do
      let
        tg t f = do
          el "p" $ do
            text t
            display (f <$> dg)
      tg "Current round: " currentRound
      el "p" $ dynText $ (\g -> [fmt|Drawn cards: {cardsDrawAtRound $ currentRound g:d} / Cards on board: {cardsOnBoardAtRound $ currentRound g:d}|]) <$> dg
      tg "Available coins: " availableCoins

    elClass "div" "gameArea" $ mdo
      (ePlayer :: _) <- elClass "div" "players" $ do
        asList <- listDyn (players <$> dg)

        evts <- dyn $ do
          ffor asList $ \l -> do
            leftmost <$> mapM (playerWidget dg dCurrentPlayerId canStealDyn currentSelection) (zip [0..] l)

        switchHold never evts

      (selectionEvent, commandFromHand) <- elClass "div" "handle" $ do
        e <- dyn (displayHand dg dCurrentPlayerId currentSelection <$> ((\(p, pID) -> (\c -> (evaluateMajorityFor c . map board . players) p == Just pID)) <$> ((,) <$> dg <*> dCurrentPlayerId)) <*> dHand)
        let (a, b) = splitE e
        a' <- switchHold never a
        b' <- switchHold never b

        pure (a', b')

      let (eSelectCard, eCommandFromPlayer :: Event t KoryoCommands) = fanEither ePlayer

      currentSelection <- foldDyn ($) NotSelecting $ leftmost [
        current ((\g -> updateSelection (\pId attack -> canBeFocused (map board (players g)) pId attack)) <$> dg) <@> eSelectCard
        , const <$> selectionEvent
        ]

      pure $ (leftmost [commandFromHand, eCommandFromPlayer], currentSelection)
  pure events

widgetBurger :: MonadWidget t m => Dynamic t Int -> m ()
widgetBurger val = do
  void $ elClass "div" "burger" $ do
    simpleList ((\x -> enumFromThenTo x (x-1) 1) <$> val) $ \dValue -> do
      el "div" $ el "span" $ display dValue

runUIDeveloper = mainWidgetWithCss css $ do
  el "table" $ do
    el "tr" $ do
      el "td" $ koryoMain (Just "Guillaume")
      el "td" $ koryoMain (Just "Cyrielle")
    el "tr" $ do
      el "td" $ koryoMain (Just "Mauricio")
      el "td" $ koryoMain (Just "Hélène")

runUI :: IO ()
runUI = mainWidgetWithCss css (koryoMain Nothing)

koryoMain player = mdo
  currentHost <- Text.takeWhile (/=':') <$> getLocationHost

  raw <- jsonWebSocket @RemoteCommand @Payload ("ws://" <> currentHost <> ":9160") $ def {
    _webSocketConfig_send = pure <$> sentEvt
    }

  pb <- getPostBuild

  let initSelectPlayer = case player of
        Nothing -> never
        Just pName -> (Login pName) <$ pb

  lastMessage <- holdDyn Nothing (_webSocket_recv raw)
  currentGame <- maybeDyn lastMessage

  evt <- dyn $ flip fmap currentGame $ \e -> case e of
    Nothing -> do
      el "p" $ text "Error / Not connected"
      el "p" $ do
        text "Login: "
        t <- textInput def

        pure (Login . Text.unpack <$> updated (value t))
    Just dg -> do
      let dCurrentPlayerId = (\(Payload _ _ i) -> i) <$> dg
      koryoCommand <- widgetGame dg
      pure $ (current (GameCommand <$> dCurrentPlayerId) <@> koryoCommand)

  eGame <- switchHold never evt

  let sentEvt = leftmost [
        eGame,
        initSelectPlayer
        ]

  pure ()

cardPicker cards pred = mdo
  currentSelection <- foldDyn (\(c, v) m -> Map.filter (/=0) $ Map.insertWith (+) c v m)  Map.empty (leftmost [
                                                                              (, 1) <$> eSelect,
                                                                              (\c -> (c, -1)) <$> eUnselect
                                                                              ])

  currentNotSelected <- foldDyn (\(c, v) m -> Map.filter (/=0) $ Map.insertWith (+) c v m)  cards (leftmost [
                                                                              (\c -> (c, -1)) <$> eSelect,
                                                                              (,1) <$> eUnselect
                                                                              ])

  eSelectNotFiltered :: Event t Card <- displayCards (constDyn mempty) currentNotSelected
  let eSelect = ffilter pred eSelectNotFiltered
  text "<->"
  eUnselect :: Event t Card <- displayCards (constDyn mempty) currentSelection

  pure (currentSelection, currentNotSelected)

handSelector :: MonadWidget t m => Bool -> Map Card Int -> m (Event t SelectedFromDraw)
handSelector majorityOf5 cards = mdo
  el "p" $ text "Select your cards"
  (currentSelection, _currentNotSelected) <- cardPicker cards (const True)

  -- TODO: check that we have the card 5
  let validSelection = ffor currentSelection $ \m -> do
        case Map.toList m of
          [(c, n)] -> Just $ SelectMany c n
          [(c, 1), (c', 1)] -> Just $ SelectTwo c c'
          _ -> Nothing

  let buttonStatus = ffor validSelection $ \sel -> case sel of
        Just (SelectTwo _ _)
          | majorityOf5 -> mempty
        Just (SelectMany _ _) -> mempty
        _ -> "disabled" =: "disabled"

  (button, _) <- elDynAttr' "button" buttonStatus $ text "Validate Selection"

  pure (flip fforMaybe id $ current validSelection <@ (domEvent Click button))

handDestroyor :: MonadWidget t m => Game -> Int -> m (Event t KoryoCommands)
handDestroyor game pId
  | selectedPlayer game /= pId = text "Wait for the other to destroy their cards" >> pure never
  | otherwise = mdo
  let cards = view (#players . ix pId . #board) game

  let
    currentNbCards = nbCards <$> newHand
    newGame = flip (set (#players . ix pId . #board)) game <$> newHand
    majorityOnNewBoard game = evaluateMajorityFor C3_SaveTwoCards (map board (players game)) == Just pId

    maxCardForMe = ffor newGame $ \game ->
      (cardsOnBoardAtRound . currentRound $ game) + bool 0 2 (majorityOnNewBoard game)

  el "p" $ do
    text "Your number of card: "
    display currentNbCards
  el "p" $ do
    text "You must have no more than: "
    display maxCardForMe

  (droppedCards, newHand) <- cardPicker cards (\c -> c /= Cm1_KillOne && c /= Cm1_FlipTwo)

  let
    -- TODO: check that we cannot be blocked because too much -1
    validSelection = (<=) <$> currentNbCards <*> maxCardForMe

  let
    buttonStatus = bool ("disabled" =: "disabled") mempty <$> validSelection

  (button, _) <- elDynAttr' "button" buttonStatus $ text "Validate Selection"

  pure $ (current (DropCards <$> droppedCards) <@ domEvent Click button)
