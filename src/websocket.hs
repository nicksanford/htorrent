{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module WebSocket (start) where
import Data.Text (Text)
import Control.Monad (forever)
import Control.Concurrent.Chan (Chan, readChan, dupChan)

import qualified Network.WebSockets as WS

import qualified Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)

application ::  Chan Text -> WS.ServerApp
application chan pending = do
    broadcastChan <- dupChan chan

    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    --  TODO: Keep track of every message which has been sent and send those messages to newly connected clients
    forever $ do
      msg <- readChan broadcastChan
      WS.sendTextData conn msg

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "web")

start :: Int -> Chan Text -> IO ()
start port chan = do
    putStrLn $ "Visualization may be seen on http://localhost:" <> show port <> "/"
    Warp.runSettings (Warp.setPort port Warp.defaultSettings)
                     (WaiWS.websocketsOr WS.defaultConnectionOptions (application chan) staticApp)
