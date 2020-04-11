{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Server where
import Data.Text (Text)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import qualified Data.Text.IO as T
import Koryo
import Data.Aeson as Aeson

import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)

data ServerState = ServerState {
  clients :: [Client],
  game :: TopLevelGame,
  currentDebugClient :: Int
  }

newServerState :: ServerState
newServerState = ServerState [] (drawPhase $ topLevel { Koryo.game = addPlayer "Guillaume" $ addPlayer "Cyrielle" $ addPlayer "Mauricio" $ addPlayer "Hélène" $ Koryo.game topLevel }) 0
  where
    topLevel = startGame 0


numClients :: ServerState -> Int
numClients = length . clients

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst) . clients

addClient :: Client -> ServerState -> ServerState
addClient client s@ServerState{clients} = s {clients = client : clients}

removeClient :: Client -> ServerState -> ServerState
removeClient client state = state {clients = filter ((/= fst client) . fst) (clients state)}

broadcast :: Text -> ServerState -> IO ()
broadcast message state = do
    T.putStrLn message
    forM_ (clients state) $ \(_, conn) -> WS.sendTextData conn message

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
    forever $ do
        state <- readMVar stateRef
        WS.sendTextData conn (Payload (Koryo.game (Server.game state)) (Koryo.handles (Server.game state) !! (min (currentDebugClient state) 3)) (currentDebugClient state))
        msg <- WS.receiveData @(Maybe KoryoCommands) conn
        case msg of
          Nothing -> putStrLn "error when decoding message"
          Just m -> case m of
            AddPlayer _s -> putStrLn "un bolos veut jouer"
            ChangePlayer i -> do
              modifyMVar_ stateRef (\s -> pure $ s {currentDebugClient = i})
            SelectHand s -> do
              modifyMVar_ stateRef (\state -> pure $ state { Server.game = attemptRevealPhase $ selectCard (currentDebugClient state) s (Server.game state)})
            EndTurn -> do
              modifyMVar_ stateRef (\state ->
                                      pure $ state {
                                       Server.game = endPlayerTurn (Server.game state)
                                       })



        print msg

instance WS.WebSocketsData (Maybe KoryoCommands) where
  fromDataMessage (WS.Text t _) = Aeson.decode t
  fromDataMessage (WS.Binary _) = error "WTF"

instance WS.WebSocketsData Game where
  toLazyByteString m = Aeson.encode m

instance WS.WebSocketsData Hand where
  toLazyByteString m = Aeson.encode m

instance WS.WebSocketsData Payload where
  toLazyByteString m = Aeson.encode m
