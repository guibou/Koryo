{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS -Wno-orphans -Wno-missing-methods -Wno-name-shadowing#-}
module Server where
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar,takeMVar,putMVar)
import Koryo
import Control.Lens
import Data.Generics.Labels()
import Data.Aeson as Aeson
import Data.List (find)
import GHC.Generics
import Control.Exception (bracket)

import qualified Network.WebSockets as WS

data ServerState = ServerState {
  clients :: [(Int, WS.Connection)],
  game :: TopLevelGame
  }
  deriving (Generic)

newServerState :: ServerState
newServerState = ServerState [] (drawPhase $ topLevel { Koryo.game = addPlayer "Guillaume" $ addPlayer "Cyrielle" $ addPlayer "Mauricio" $ addPlayer "Hélène" $ Koryo.game topLevel })
  where
    topLevel = startGame 0


broadcastPayload :: MVar ServerState -> IO ()
broadcastPayload stateRef = bracket (takeMVar stateRef)
  (putMVar stateRef)
  $ \state -> do
    forM_ (clients state) $ \(pId, conn) -> do
      let
        message = Payload (Koryo.game (Server.game state)) (Koryo.handles (Server.game state) !! pId) pId
      WS.sendTextData conn message

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application stateRef pending = do
    conn <- WS.acceptRequest pending
    putStrLn "Connection accepted"
    WS.forkPingThread conn 30
    -- WS.withPingThread conn 30 (return ()) $ do

    -- TODO: take disconnection into account
    let
      loop = do
        state <- readMVar stateRef
        msg <- WS.receiveData @(Maybe KoryoCommands) conn
        case msg of
          Nothing -> putStrLn "error when decoding message"
          Just m -> case m of
            Login loginName -> case find (\(_pId, player) -> name player == loginName) $ (zip [0..] (view (#game . #game . #players) state)) of
              Nothing -> do
                print ("Login error with" <> loginName)
                loop
              Just (pId, _) -> do
                modifyMVar_ stateRef $ \state -> do
                  pure $ state {
                    clients = (pId, conn):clients state
                    }

                broadcastPayload stateRef
                mainLoop pId
            _ -> error "You should not send any game command without login"

      mainLoop pId = forever $ do
        msg <- WS.receiveData @(Maybe KoryoCommands) conn
        print (pId, msg)
        case msg of
          Nothing -> putStrLn "error when decoding message"
          Just m -> case m of
            Login _ -> error "WTF"
            SelectHand s -> do
              modifyMVar_ stateRef (\state -> pure $ state { Server.game = attemptRevealPhase $ selectCard pId s (Server.game state)})
            EndTurn -> do
              modifyMVar_ stateRef (\state ->
                                      pure $ state {
                                       Server.game = endPlayerTurn (Server.game state)
                                       })
            TakeCoinCommand -> do
              modifyMVar_ stateRef (\state -> pure $ state { Server.game = takeCoinInTheBank (Server.game state) pId})
            DestroyCardCommand -> do
              modifyMVar_ stateRef (\state -> pure $ state { Server.game = destroyAPersonalCard (Server.game state) pId})

            StealACoinToPlayer i -> do
              modifyMVar_ stateRef (\state -> pure $ state { Server.game = stealACoinToPlayer (Server.game state) pId i})

            FireCommand c -> do
              modifyMVar_ stateRef (\state -> pure $ state { Server.game = fireCommand (Server.game state) pId c})
            FlipCommand c c' -> do
              modifyMVar_ stateRef (\state -> pure $ state { Server.game = flipCommand (Server.game state) pId c c'})
            DropCards dp -> do
              modifyMVar_ stateRef (\state -> pure $ state { Server.game = dropCards (Server.game state) pId dp})

        broadcastPayload stateRef
    loop

instance WS.WebSocketsData (Maybe KoryoCommands) where
  fromDataMessage (WS.Text t _) = Aeson.decode t
  fromDataMessage (WS.Binary _) = error "WTF"

instance WS.WebSocketsData Game where
  toLazyByteString m = Aeson.encode m

instance WS.WebSocketsData Hand where
  toLazyByteString m = Aeson.encode m

instance WS.WebSocketsData Payload where
  toLazyByteString m = Aeson.encode m
