{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Data.Text (Text)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, readChan, writeChan, dupChan, newChan)
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

import qualified Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)

-- For htorrent I need to be able to have all peers have a channel which will receive every single have message which is received by the manager thread.
-- I need to have every thread (both manager and otherwise) be able to send messages to the websocket thread to broadcast messages to all connected clients.
application ::  Chan Text -> WS.ServerApp
application chan pending = do
    broadcastChan <- dupChan chan

    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    WS.sendTextData conn ("HELLO! U just connected... waiting for keyboard input" :: Text)
    forever $ do
      msg <- readChan broadcastChan
      WS.sendTextData conn msg


staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "web")

start :: Int -> Chan Text -> IO ()
start port chan = do
    _<- forkIO $ Warp.runSettings (Warp.setPort port Warp.defaultSettings)
                                  (WaiWS.websocketsOr WS.defaultConnectionOptions (application chan) staticApp)
    forever $ T.getLine >>= writeChan chan

    -- get keyboard input
    -- write to chan
    -- call broadcast
    -- we can also experiment with the dup chan method
    -- 
main = newChan >>= start 9160
