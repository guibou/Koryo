{-# LANGUAGE ViewPatterns #-}
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
{-# OPTIONS -Wno-name-shadowing #-}

module UIReflex.UI (runUI, runUIDeveloper) where

import UIReflex.CSS

import Reflex.Dom hiding (FireCommand)
import qualified Data.Text as Text
import Control.Monad (void)
import PyF (fmt)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Generics.Labels()
import Control.Lens
import Data.Text.Encoding

import Koryo
import Data.Text (Text)
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

projectCardSelection :: Int -> CardSelectionMode -> Set Card
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
playerWidget :: MonadWidget t m => Dynamic t Game -> Dynamic t (Int -> Bool) -> Dynamic t CardSelectionMode -> (Int, Dynamic t Player) -> m (Event t (Either (Int, Card) KoryoCommands))
playerWidget game canStealDyn cardSelectDyn (playerNumber, player) = do
  (_, eStealCoin) <- el' "div" $ mdo
    let
    elClass "div" "name" $ do
      dynText (Text.pack . name <$> player)
      dyn_ (ffor game $ \game -> if currentFirstPlayer game == playerNumber
                                then do
               text " - "
               elClass "span" "firstPlayer" $ text "Premier joueur"
                                else blank
              )
      dyn_ (ffor game $ \game -> if selectedPlayer game == playerNumber
                                    then do
               text " - "
               elClass "span" "currentPlayer" $ text "Joueur actuel"
                                    else blank
              )
      dynText (ffor game $ \game ->
                  let currentScore = computeScores (players game) !! playerNumber
                  in [fmt| - Score: {currentScore}|])
    eStealCoin <- elClass "div" "coin" $ do
      text "Pieces: "
      displayCoins (nbCoins <$> player)
      let enabled = ffor canStealDyn $ \canSteal -> bool ("disabled" =: "disabled") mempty (canSteal playerNumber)
      (b, _) <- elDynAttr' "button" enabled (text "Voler une pièce !")

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
updateSelection canBeFocused sel@(target, _) currentSel@(Selecting (SelectingFire b))
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
    flip mapM enumerated $ \card -> do
      let dCount = fromMaybe 0 . Map.lookup card <$> cards
      let selected selStatus count = cls <> ("data-count" =: Text.pack (show count))
            where
              cls
                | card `Set.member` selStatus = ("class" =: ("selected card " <> (Text.pack $ show card)))
                | otherwise = ("class" =: ("card " <> (Text.pack $ show card)))
      (e, _) <- elDynAttr' "div" (selected <$> selectedCards <*> dCount) $ do
            elClass "div" "help" $ do
              elClass "div" "description" $ text $ description card
              elClass "div" "marker" $ text "?"

            widgetBurger card dCount
      let cardClick = domEvent Click e
      pure $ (card <$ cardClick)

  pure $ leftmost selectCardEvent

description :: Card -> Text
description = \case
  C1_GivePrio -> "Résoud les égalités"
  C2_Ninja -> "Voler une pièce. Protège contre les échanges si pas de 7."
  C3_SaveTwoCards -> "Garde 2 cartes de plus."
  C4_KillMinusOne -> "Detruit un -1."
  C5_TakeTwoDifferent -> "Pioche deux cartes differentes."
  C6_Bank -> "Pioche une pièce"
  C7_Warrior -> "Protège contre les -1 rouges."
  C8_DrawTwoMore -> "Pioche une carte de plus."
  C9_DoNothing -> "Rapporte des points."
  Cm1_FlipTwo -> "Echange deux cartes ciblées si non protegées par le 2."
  Cm1_KillOne -> "Detruit une carte ciblée si non protegée par le 7."

selectToCommand :: CardSelectionMode -> Maybe KoryoCommands
selectToCommand (Selecting (SelectingFlip (Just (a, Just b)))) = Just (FlipCommand a b)
selectToCommand (Selecting (SelectingFire (Just a))) = Just (FireCommand a)
selectToCommand _ = Nothing

displayHand :: forall t m. MonadWidget t m
            => Dynamic t Game
            -> Dynamic t Int
            -> Dynamic t CardSelectionMode
            -> (Card -> Bool)
            -> Hand
            -> m (Event t CardSelectionMode, Event t KoryoCommands)
displayHand (traceDyn "Game"->dGame) (traceDyn "currentPlayer"->dCurrentPlayerId) (traceDyn "CurrentSelection"->dCurrentSelection) majoritySelector = \case
  NothingToDo -> elClass "div" "roundedBlock" $ text "Votre tour est fini. Attendez le prochain." >> pure (never, never)
  WaitingForDestroying -> elClass "div" "roundedBlock" $ do
    text "Phase de destruction"

    eDestroy <- dyn $ (handDestroyor <$> dGame <*> dCurrentPlayerId)
    eDestroyS <- switchHold never eDestroy

    pure (never, eDestroyS)
  Draw m -> do
    e <- handSelector (majoritySelector C5_TakeTwoDifferent) m
    pure (never, SelectHand <$> e)
  Selected sel -> elClass "div" "roundedBlock" $ do
    text "Vous aves selectionné des cartes. Attendez votre tour maintenant."
    void $ displayCards (constDyn mempty) (constDyn (selectionToMap sel))
    pure (never, never)
  DoActions actions -> do
    let
      simpleText t = text t >> pure (never, never)
      btn t enabled event = do
        (b, _) <- elDynAttr' "button" (bool ("disabled" =: "disabled") mempty <$> enabled) $ text t
        pure (event <$ (domEvent Click b))

      selectCommand = selectToCommand <$> dCurrentSelection
      isFlipping = ffor dCurrentSelection $ \case
        Selecting (SelectingFlip _) -> True
        _ -> False
      isFiring = ffor dCurrentSelection $ \case
        Selecting (SelectingFire _) -> True
        _ -> False

      -- TODO: check that some action can be done

    let
      blinkingClass = bool "" "blinking"

    e <- mapM (el "div"$) $
      [
        do
          if flipAction actions /= 0
          then elClass "div" "roundedBlock" $ do
            el "p" $ text "Vous pouvez échanger des cartes:"
            e <- elDynClass "span" (blinkingClass <$> isFlipping) $ do
              btn ([fmt|Echanger deux cartes {flipAction actions}|]) (constDyn True) (Selecting (SelectingFlip Nothing))
            runCommand <- btn "Confirmer l'échange" (ffor selectCommand $ \case
                                 Just (FlipCommand _ _) -> True
                                 _ -> False) ()
            pure (e, fmapMaybe id (current selectCommand <@ runCommand))
          else pure (never, never)
                   ,
        do
           if kill actions /= 0
             then elClass "div" "roundedBlock" $ do
               el "p" $ text "Vous pouvez détruire des cartes:"
               e <- elDynClass "span" (blinkingClass <$> isFiring) $ do
                 btn [fmt|Détruire une carte {kill actions}|] (constDyn True) (Selecting (SelectingFire Nothing))
               runCommand <- btn "Confirmer la destruction de la carte ciblée." (ffor selectCommand $ \case
                                     Just (FireCommand _) -> True
                                     _ -> False) ()
               pure (e, fmapMaybe id (current selectCommand <@ runCommand))
             else pure (never, never)
                    ,
        -- TODO: test that there are available coins
        if majoritySelector C2_Ninja && stealCoin actions
             then elClass "div" "roundedBlock" $ simpleText "Vous pouvez volez une pièce à un autre joueur. Regardez à coté de leur nom."
             else pure (never, never)
                    ,
        -- TODO: check that there is coin in the bank
        if takeCoin actions && majoritySelector C6_Bank
             then elClass "div" "roundedBlock" $ do
             e <- btn [fmt|Prendre une pièce dans la banque|] (constDyn True) TakeCoinCommand
             pure (never, e)
             else pure (never, never)
                    ,
        -- TODO: check that there is something to destroy
        if destroyCard actions && majoritySelector C4_KillMinusOne
             then elClass "div" "roundedBlock" $ do
               e <- btn "Detruire une carte -1" (constDyn True) DestroyCardCommand
               pure (never, e)
             else pure (never, never)
                    ]

    let (selectionEvent, commandEvent) = unzip e

    (bEndTurn, _) <- elClass "div" "roundedBlock" $ do
         el "p" $ text "C'est votre tour. Réalisez les differentes actions proposée au dessus, puis terminez votre tour."
         el' "button" $ text "Fin du tour."

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
    dg <- holdUniqDyn ((\(Payload g _ _) -> g) <$> dPayload)
    dHand <- holdUniqDyn ((\(Payload _ h _) -> h) <$> dPayload)
    dCurrentPlayerId <- holdUniqDyn ((\(Payload _ _ i) -> i) <$> dPayload)
    dBoard <- holdUniqDyn (map board . players <$> dg)

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
      tg "Tour actuel: " currentRound
      el "p" $ dynText $ (\g -> [fmt|Carte piochées: {cardsDrawAtRound $ currentRound g:d} / Carte à garder: {cardsOnBoardAtRound $ currentRound g:d}|]) <$> dg

      displayCoins (availableCoins <$> dg)

    elClass "div" "gameArea" $ mdo
      (ePlayer :: _) <- elClass "div" "players" $ do
        asList <- listDyn (players <$> dg)

        evts <- dyn $ do
          ffor ((,) <$> asList <*> dCurrentPlayerId) $ \(l, currentPlayerId) -> do
            let
              listWithPlayerIdx = zip [0..] l

              -- permutation up to the next player
              newList = drop currentPlayerId listWithPlayerIdx <> take currentPlayerId listWithPlayerIdx

            leftmost <$> mapM (playerWidget dg canStealDyn currentSelection) newList

        switchHold never evts

      (selectionEvent, commandFromHand) <- elClass "div" "handle" $ do
        e <- dyn (traceDynWith (const "displayHand") $ displayHand dg dCurrentPlayerId currentSelection <$> ((\(p, pID) -> (\c -> (evaluateMajorityFor c) p == Just pID)) <$> ((,) <$> (traceDyn "board" dBoard) <*> (traceDyn "pid" dCurrentPlayerId))) <*> (traceDyn "hand" dHand))
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

displayCoins :: MonadWidget t m => Dynamic t Int -> m ()
displayCoins currentCount = elClass "div" "coins" $ flip mapM_ [1..8] $ \c -> do
  let
    cls c c'
      | c <= c' = "visible"
      | otherwise = ""
  elDynClass "div" (cls c <$> currentCount) $ el "span" $ text (Text.pack . show $ c)

widgetBurger :: MonadWidget t m => Card -> Dynamic t Int -> m ()
widgetBurger card val = do
  void $ elDynAttr "div" ((\c -> "class" =: "burger" <> "data-count" =: Text.pack (show c)) <$> val) $ do
    let cc = cardCount card
    flip mapM [cc,(cc-1)..1] $ \i -> do
      elDynClass "div" ((\v -> case compare i v of
                           LT -> "lt visible"
                           EQ -> "eq visible"
                           GT -> "gt") <$> val) $ el "span" $ text (Text.pack $ show i)

runUIDeveloper :: IO ()
runUIDeveloper = mainWidgetWithHead koryoHead $ do
  el "table" $ do
    el "tr" $ do
      el "td" $ koryoMain (Just "Guillaume")
      el "td" $ koryoMain (Just "Cyrielle")
    el "tr" $ do
      el "td" $ koryoMain (Just "Mauricio")
      el "td" $ koryoMain (Just "Hélène")

runUI :: Maybe String -> IO ()
runUI p = mainWidgetWithHead koryoHead (koryoMain p)

koryoHead :: _ => m ()
koryoHead = do
  el "title" $ text "Koryo - Un jeu trop bien"
  currentHost <- Text.takeWhile (/=':') <$> getLocationHost
  elAttr "style" ("type" =: "text/css") $ text $ decodeUtf8 (css currentHost)

  pure ()

koryoMain :: MonadWidget t m => Maybe String -> m ()
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
    Nothing -> elClass "div" "roundedBlock" $ do
      el "p" $ text "Pour commencer une partie, entrez votre nom:"
      el "p" $ do
        text "Login: "
        t <- inputElement def

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

cardPicker :: _ => Map Card Int
           -> (Card -> Bool)
           -> m (Dynamic t (Map Card Int), Dynamic t (Map Card Int))
cardPicker cards pred = mdo
  currentSelection <- foldDyn (\(c, v) m -> Map.insertWith (+) c v m)  Map.empty (leftmost [
                                                                                     (, 1) <$> eSelect,
                                                                                     (\c -> (c, -1)) <$> eUnselect
                                                                                     ])

  currentNotSelected <- foldDyn (\(c, v) m -> Map.insertWith (+) c v m)  cards (leftmost [
                                                                                   (\c -> (c, -1)) <$> eSelect,
                                                                                   (,1) <$> eUnselect
                                                                                   ])

  eSelectNotFiltered :: Event t Card <- displayCards (constDyn mempty) currentNotSelected
  let eSelect = ffilter pred eSelectNotFiltered
  text "<->"
  eUnselect :: Event t Card <- displayCards (constDyn mempty) currentSelection

  pure (currentSelection, currentNotSelected)

handSelector :: MonadWidget t m => Bool -> Map Card Int -> m (Event t SelectedFromDraw)
handSelector majorityOf5 cards = elClass "div" "roundedBlock" $ mdo
  el "p" $ text "Selection des cartes. La ligne du haut correspond à votre pioche. La ligne du bas correspond à votre selection. Il suffit de cliquer pour passer les cartes d'une ligne à l'autre."
  (currentSelection, _currentNotSelected) <- cardPicker cards (const True)

  -- TODO: check that we have the card 5
  let validSelection = ffor currentSelection $ \m -> do
        -- Shrink the card list out of the 0 values
        case Map.toList (Map.filter (/=0) m) of
          [(c, n)] -> Just $ SelectMany c n
          [(c, 1), (c', 1)] -> Just $ SelectTwo c c'
          _ -> Nothing

  let buttonStatus = ffor validSelection $ \sel -> case sel of
        Just (SelectTwo _ _)
          | majorityOf5 -> mempty
        Just (SelectMany _ _) -> mempty
        _ -> "disabled" =: "disabled"

  (button, _) <- elDynAttr' "button" buttonStatus $ text "Valider la selection"

  pure (flip fforMaybe id $ current validSelection <@ (domEvent Click button))

handDestroyor :: MonadWidget t m => Game -> Int -> m (Event t KoryoCommands)
handDestroyor game pId
  | selectedPlayer game /= pId = elDynClass "div" "roundedBlock" $ text "Phase de défausse. Attendez votre tour." >> pure never
  | otherwise = elDynClass "div" "roundedBlock" $ mdo
  let cards = view (#players . ix pId . #board) game
  el "p" $ text "Choissizer les cartes à supprimer. La ligne du haut correspond aux cartes à conserver."

  let
    currentNbCards = nbCards <$> newHand
    newGame = flip (set (#players . ix pId . #board)) game <$> newHand
    majorityOnNewBoard game = evaluateMajorityFor C3_SaveTwoCards (map board (players game)) == Just pId

    maxCardForMe = ffor newGame $ \game ->
      (cardsOnBoardAtRound . currentRound $ game) + bool 0 2 (majorityOnNewBoard game)

  el "p" $ do
    text "Nombre actuel de carte : "
    display currentNbCards
  el "p" $ do
    text "Vous pouvez en conserver : "
    display maxCardForMe

  (droppedCards, newHand) <- cardPicker cards (\c -> c /= Cm1_KillOne && c /= Cm1_FlipTwo)

  let
    -- TODO: check that we cannot be blocked because too much -1
    validSelection = (<=) <$> currentNbCards <*> maxCardForMe

  let
    buttonStatus = bool ("disabled" =: "disabled") mempty <$> validSelection

  (button, _) <- elDynAttr' "button" buttonStatus $ text "Valider la selection"

  pure $ (current (DropCards <$> droppedCards) <@ domEvent Click button)

{-
TODOs:

Issues noted by my wife:

- personal board is too small with respect to others
- Use my own assets for cards

My TODO:

- tests ;)
  (Damned,
- more robust server
- automatic actions

GAME FEATURES:
- see the number of card drawn by others
-

-}