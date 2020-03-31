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
import Text.Read
import Data.Maybe (fromMaybe)
import Control.Monad (zipWithM_)
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text.Encoding
import Reflex.Collection

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

displayHand :: MonadWidget t m => Hand -> m (Event t KoryoCommands)
displayHand NothingToDo = text "Wait until it is your turn" >> pure never
displayHand (Draw m) = fmap SelectHand <$> handSelector m
displayHand (Selected sel) = do
  text "You selected some cards"
  void $ displayCards (constDyn (selectionToMap sel))
  pure never
displayHand (DoActions _) = text "Whaaa, it's your turn, you can do some actions, perhaps..." >> pure never

widgetGame :: MonadWidget t m => Dynamic t Payload -> m (Event t KoryoCommands)
widgetGame dPayload = do
  let dg = (\(Payload g _) -> g) <$> dPayload
  let dHand = (\(Payload _ h) -> h) <$> dPayload

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
    display (computeScores . players <$> dg)

  (ePlayer :: _) <- elClass "div" "players" $ do
    asList <- listDyn (players <$> dg)

    evts <- dyn $ do
      ffor asList $ \l -> do
        leftmost <$> mapM playerWidget (zip [0..] l)

    switchHold never evts

  handSelection <- elClass "div" "handle" $ do
    e <- dyn (displayHand <$> dHand)
    switchHold never e

  pure $ leftmost [handSelection, ePlayer]

widgetBurger :: MonadWidget t m => Dynamic t Int -> m ()
widgetBurger count = do
  void $ elClass "div" "burger" $ do
    simpleList ((\x -> enumFromThenTo x (x-1) 1) <$> count) $ \dValue -> do
      el "div" $ el "span" $ display dValue

go :: IO ()
go = mainWidgetWithCss css $ mdo
  raw <- jsonWebSocket @KoryoCommands @Payload "ws://localhost:9160" $ def {
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

handSelector :: MonadWidget t m => Map Card Int -> m (Event t SelectedFromDraw)
handSelector cards = mdo
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

  -- TODO: grey out the button
  b <- button "Validate Selection"

  pure (flip fforMaybe id $ current validSelection <@ b)
