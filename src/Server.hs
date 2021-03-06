{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS -Wno-orphans -Wno-missing-methods #-}
module Server where
import Control.Monad (forM, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_)
import Koryo
import Control.Lens
import Data.Generics.Labels()
import Data.Aeson as Aeson
import Data.List (find)
import GHC.Generics
import Control.Exception (try, SomeException)
import Data.Maybe (catMaybes)
import System.Random

import qualified Network.WebSockets as WS

data ServerState = ServerState {
  clients :: [(Int, WS.Connection)],
  game :: TopLevelGame
  }
  deriving (Generic)

newServerState :: [String] -> StdGen -> ServerState
newServerState players gen = ServerState [] (drawPhase $
                                             over #game (flip (foldr addPlayer) players) (startGame gen)
                                            )


broadcastPayload :: MVar ServerState -> IO ()
broadcastPayload stateRef = modifyMVar_ stateRef
  $ \state -> do
    putStrLn ("--------")
    print (length (clients state))
    print (Server.game state)

    newClients <- forM (clients state) $ \(pId, conn) -> do
      let
        message = Payload (Koryo.game (Server.game state)) (Koryo.handles (Server.game state) !! pId) pId
      res <- try @SomeException $ WS.sendTextData conn message

      case res of
        Right () -> pure $ Just (pId, conn)
        Left e -> do
          print ("error sending to", pId, res, e)
          pure $ Nothing

    pure $ state { clients = catMaybes newClients }

startServer :: [String] -> IO ()
startServer players = do
  gen <- newStdGen
  print gen
  state <- newMVar (newServerState players gen)
  WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application stateRef pending = do
    conn <- WS.acceptRequest pending
    putStrLn "Connection accepted"
    -- Disable ping thread for now, I don't know if that's useful
    -- Player can reload the game if needed
    -- WS.forkPingThread conn 30
    -- WS.withPingThread conn 30 (return ()) $ do

    let
      modifyGame f = modifyMVar_ stateRef $ \state -> pure (over #game f state)

      loop :: IO ()
      loop = forever $ do
        msg <- WS.receiveData @(Maybe RemoteCommand) conn
        print msg
        case msg of
          Nothing -> putStrLn "error when decoding message"
          Just (Login loginName) -> modifyMVar_ stateRef $ \state -> do
            case find (\(_pId, player) -> name player == loginName) $ (zip [0..] (view (#game . #game . #players) state)) of
              Nothing -> do
                print ("Login error with" <> loginName)
                pure state
              Just (pId, _) -> do
                print ("Logged", (pId, loginName))
                pure $ over #clients ((pId, conn):) state
          Just (GameCommand pId command) -> modifyGame (executeCommand pId command)

        broadcastPayload stateRef

    t <- try @SomeException $ loop
    print ("Unexpected end of the loop", t)

executeCommand :: Int
                        -> KoryoCommands -> TopLevelGame -> TopLevelGame
executeCommand pId command game = case command of
  -- Draw phase
  SelectHand s -> attemptRevealPhase $ selectCard pId s game

  -- Action Phase
  EndTurn -> endPlayerTurn pId game
  TakeCoinCommand -> takeCoinInTheBank game pId
  DestroyCardCommand -> destroyAPersonalCard game pId
  StealACoinToPlayer i -> stealACoinToPlayer game pId i
  FireCommand c -> fireCommand game pId c
  FlipCommand c c' -> flipCommand game pId c c'

  -- Drop phase
  DropCards dp -> dropCards game pId dp

instance WS.WebSocketsData (Maybe RemoteCommand) where
  fromDataMessage (WS.Text t _) = Aeson.decode t
  fromDataMessage (WS.Binary _) = error "WTF"

instance WS.WebSocketsData Game where
  toLazyByteString m = Aeson.encode m

instance WS.WebSocketsData Hand where
  toLazyByteString m = Aeson.encode m

instance WS.WebSocketsData Payload where
  toLazyByteString m = Aeson.encode m
