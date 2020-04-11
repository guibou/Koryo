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
{-# LANGUAGE NoMonomorphismRestriction #-}

module UIReflex.UI (go) where

import UIReflex.CSS

import Reflex.Dom
import qualified Data.Text as Text
import Control.Monad (void)
import PyF (fmt)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text.Encoding
import Data.Traversable (for)
import Data.Bool (bool)

import Koryo
import Assets

data GameState = NotPlaying
  deriving (Show)

listDyn :: MonadWidget t m => Dynamic t [a] -> m (Dynamic t [Dynamic t a])
listDyn input = do
  let
    onTheList l = map (\i -> (!!i) <$> input)  [0..(length l - 1)]
    newDyn = onTheList <$> input

    f a b = length a == length b

  holdUniqDynBy f newDyn

playerWidget :: MonadWidget t m => (Int, Dynamic t Player) -> m (Event t KoryoCommands)
playerWidget (playerNumber, player) = do
  (block, _) <- el' "div" $ do
    elClass "div" "name" $ dynText (Text.pack . name <$> player)
    elClass "div" "coin" $ display (nbCoins <$> player)

    void $ displayCards (board <$> player)
  pure (ChangePlayer playerNumber <$ domEvent Click block)

displayCards :: MonadWidget t m => Dynamic t (Map Card Int) -> m (Event t (Map Card Int))
displayCards cards = do
  elClass "div" "cards" $ do
    listViewWithKey cards $ \card dCount -> do
      elClass "div" "card" $ do
        e <- case Map.lookup card card_images of
          Nothing -> text (Text.pack . show $ card) >> pure never
          Just t -> do
            (e, _) <- elAttr' "img" (Map.fromList [
                             ("src", "data:image/png;base64," <> (decodeUtf8 t))
                               --("width", "100")
                             ]) $ pure ()
            let cardClick = domEvent Click e
            pure $ 1 <$ cardClick
        widgetBurger dCount
        pure e

displayHand :: forall t m. MonadWidget t m => (Card -> Bool) -> Hand -> m (Event t KoryoCommands)
displayHand _ NothingToDo = text "Wait until it is your turn" >> pure never
displayHand majoritySelector (Draw m) = fmap SelectHand <$> handSelector (majoritySelector C5_TakeTwoDifferent) m
displayHand _ (Selected sel) = do
  text "You selected some cards"
  void $ displayCards (constDyn (selectionToMap sel))
  pure never
displayHand majoritySelector (DoActions actions) = do
  e <- for actions $ \a -> do
    let
      -- TODO: check that some action can be done
      (t, enabled, action :: Event t () -> Event t KoryoCommands) = case a of
        -- TODO: not handled yet
        Flip i -> (text [fmt|Flip {i}|], (i /= 0), const never)
        -- TODO: not handled yet
        Kill i -> (text [fmt|Kill {i}|], (i /= 0), const never)
        -- TODO: not handled yet
        StealCoin -> (text "StealCoin", majoritySelector C2_Ninja, const never)
        -- TODO: check that there is coin in the bank
        TakeCoin -> (text "TakeCoin", majoritySelector C6_Bank, fmap (const TakeCoinCommand))
        -- TODO: check that there is something to destroy
        DestroyCard -> (text "DestroyCard", majoritySelector C4_KillMinusOne, fmap (const DestroyCardCommand))
    (b, _) <- elAttr' "button" (bool ("disabled" =: "disabled") mempty enabled) t

    pure (action (domEvent Click b))

  (b, _) <- el' "button" $ text "End Turn"

  pure $ leftmost [
    EndTurn <$ domEvent Click b,
    leftmost e
    ]

widgetGame :: MonadWidget t m => Dynamic t Payload -> m (Event t KoryoCommands)
widgetGame dPayload = do
  let dg = (\(Payload g _ _) -> g) <$> dPayload
  let dHand = (\(Payload _ h _) -> h) <$> dPayload
  let dCurrentPlayerId = (\(Payload _ _ i) -> i) <$> dPayload

  display (phase <$> dg)

  elClass "div" "gameStatus" $ do
    let
      tg t f = do
        el "p" $ do
          text t
          display (f <$> dg)
    tg "Current round: " currentRound
    el "p" $ dynText $ (\g -> [fmt|{cardsDrawAtRound $ currentRound g:d} / {cardsOnBoardAtRound $ currentRound g:d}|]) <$> dg
    tg "Available coins: " availableCoins
    tg "Current first player: " currentFirstPlayer
    tg "Current player: " selectedPlayer
    display (computeScores . players <$> dg)

  (ePlayer :: _) <- elClass "div" "players" $ do
    asList <- listDyn (players <$> dg)

    evts <- dyn $ do
      ffor asList $ \l -> do
        leftmost <$> mapM playerWidget (zip [0..] l)

    switchHold never evts

  handSelection <- elClass "div" "handle" $ do
    e <- dyn (displayHand <$> ((\(p, pID) -> (\c -> (evaluateMajorityFor c . map board . players) p == Just pID)) <$> ((,) <$> dg <*> dCurrentPlayerId)) <*> dHand)
    switchHold never e

  pure $ leftmost [handSelection, ePlayer]

widgetBurger :: MonadWidget t m => Dynamic t Int -> m ()
widgetBurger val = do
  void $ elClass "div" "burger" $ do
    simpleList ((\x -> enumFromThenTo x (x-1) 1) <$> val) $ \dValue -> do
      el "div" $ el "span" $ display dValue

go :: IO ()
go = mainWidgetWithCss css $ mdo
  currentHost <- Text.takeWhile (/=':') <$> getLocationHost

  raw <- jsonWebSocket @KoryoCommands @Payload ("ws://" <> currentHost <> ":9160") $ def {
    _webSocketConfig_send = pure <$> sentEvt
    }

  lastMessage <- holdDyn Nothing (_webSocket_recv raw)
  currentGame <- maybeDyn lastMessage

  evt <- dyn $ flip fmap currentGame $ \e -> case e of
    Nothing -> text "Error / Not connected" >> pure never
    Just dg -> widgetGame dg

  eGame <- switchHold never evt

  let sentEvt = leftmost [
        eGame
        ]

  pure ()

handSelector :: MonadWidget t m => Bool -> Map Card Int -> m (Event t SelectedFromDraw)
handSelector majorityOf5 cards = mdo
  currentSelection <- foldDyn (\m' m -> Map.filter (/=0) $ Map.unionWith (+) m m')  Map.empty (leftmost [
                                                                              eSelect,
                                                                              fmap negate <$> eUnselect
                                                                              ])

  currentNotSelected <- foldDyn (\m' m -> Map.filter (/=0) $ Map.unionWith (+) m m')  cards (leftmost [
                                                                              fmap negate <$> eSelect,
                                                                              eUnselect
                                                                              ])

  eSelect <- displayCards currentNotSelected
  eUnselect <- displayCards currentSelection

  -- TODO: check that we have the card 5
  let validSelection = ffor currentSelection $ \m -> do
        case Map.toList m of
          [(c, n)] -> Just $ SelectMany c n
          [(c, 1), (c', 1)] -> Just $ SelectTwo c c'
          _ -> Nothing

  display validSelection

  let buttonStatus = ffor validSelection $ \sel -> case sel of
        Just (SelectTwo _ _)
          | majorityOf5 -> mempty
        Just (SelectMany _ _) -> mempty
        _ -> "disabled" =: "disabled"

  -- TODO: grey out the button
  (button, _) <- elDynAttr' "button" buttonStatus $ text "Validate Selection"

  pure (flip fforMaybe id $ current validSelection <@ (domEvent Click button))
