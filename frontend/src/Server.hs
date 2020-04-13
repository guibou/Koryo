{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
module Server where
import Control.Monad (forM, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Koryo
import Control.Lens
import Data.Generics.Labels()
import Data.Aeson as Aeson
import Data.List (find)
import GHC.Generics
import Control.Monad (void)
import Control.Exception (try, SomeException)
import Data.Maybe (catMaybes)

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
broadcastPayload stateRef = modifyMVar_ stateRef
  $ \state -> do
    putStrLn ("--------")
    print (length (clients state))

    newClients <- forM (clients state) $ \(pId, conn) -> do
      let
        message = Payload (Koryo.game (Server.game state)) (Koryo.handles (Server.game state) !! pId) pId
      void $ try @SomeException $ WS.sendTextData conn message
      res <- try @SomeException $ WS.sendTextData conn message

      case res of
        Right () -> pure $ Just (pId, conn)
        Left e -> do
          print ("error sending to", pId, res, e)
          pure $ Nothing

    pure $ state { clients = catMaybes newClients }

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

    let
      modifyGame f = modifyMVar_ stateRef $ \state -> pure (over #game f state)

      loop :: IO ()
      loop = forever $ do
        msg <- WS.receiveData @(Maybe RemoteCommand) conn
        print msg
        case msg of
          Nothing -> putStrLn "error when decoding message"
          Just (Login loginName) -> do
            statee <- readMVar stateRef
            case find (\(_pId, player) -> name player == loginName) $ (zip [0..] (view (#game . #game . #players) statee)) of
              Nothing -> do
                print ("Login error with" <> loginName)
              Just (pId, _) -> do
                print ("Logged", (pId, loginName))
                modifyMVar_ stateRef $ \state -> do
                  pure $ state {
                    clients = (pId, conn):clients state
                    }
          Just (GameCommand pId command) -> case command of
            SelectHand s -> modifyGame $ \game -> attemptRevealPhase $ selectCard pId s game
            EndTurn -> modifyGame $ \game -> endPlayerTurn game
            TakeCoinCommand -> modifyGame $ \game -> takeCoinInTheBank game pId
            DestroyCardCommand -> modifyGame $ \game -> destroyAPersonalCard game pId
            StealACoinToPlayer i -> modifyGame $ \game -> stealACoinToPlayer game pId i
            FireCommand c -> modifyGame $ \game -> fireCommand game pId c
            FlipCommand c c' -> modifyGame $ \game -> flipCommand game pId c c'
            DropCards dp -> modifyGame $ \game -> dropCards game pId dp

        broadcastPayload stateRef

    t <- try @SomeException $ loop
    print ("Unexpected end of the loop", t)

instance WS.WebSocketsData (Maybe RemoteCommand) where
  fromDataMessage (WS.Text t _) = Aeson.decode t
  fromDataMessage (WS.Binary _) = error "WTF"

instance WS.WebSocketsData Game where
  toLazyByteString m = Aeson.encode m

instance WS.WebSocketsData Hand where
  toLazyByteString m = Aeson.encode m

instance WS.WebSocketsData Payload where
  toLazyByteString m = Aeson.encode m
