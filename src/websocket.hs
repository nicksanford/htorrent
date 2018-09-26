{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module WebSocket (start) where
import Control.Exception (finally)
import Data.Text (Text)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, forkIO)
import Control.Concurrent.Chan (Chan, readChan, writeChan, dupChan)
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS
import Data.UUID.V4 (nextRandom)
import Data.UUID (UUID)

import qualified Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)

type Client = (UUID, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

-- For htorrent I need to be able to have all peers have a channel which will receive every single have message which is received by the manager thread.
-- I need to have every thread (both manager and otherwise) be able to send messages to the websocket thread to broadcast messages to all connected clients.
application :: MVar ServerState -> Chan Text -> WS.ServerApp
application state chan pending = do
    broadcastChan <- dupChan chan

    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    uuid <- nextRandom

    flip finally (disconnect (uuid, conn)) $ do
      modifyMVar_ state $ \x -> return $ addClient (uuid, conn) x
      WS.sendTextData conn ("HELLO! U just connected... waiting for keyboard input" :: Text)
      forever $ do
        msg <- readChan broadcastChan
        WS.sendTextData conn msg

  where disconnect client =
          modifyMVar_ state $ \s -> return $ removeClient client s

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "web")

start :: Int -> Chan Text -> IO ()
start port chan = do
    state <- newMVar newServerState
    _<- forkIO $ Warp.runSettings (Warp.setPort port Warp.defaultSettings)
                                  (WaiWS.websocketsOr WS.defaultConnectionOptions (application state chan) staticApp)
    forever $ T.getLine >>= writeChan chan

    -- get keyboard input
    -- write to chan
    -- call broadcast
    -- we can also experiment with the dup chan method
    -- 
