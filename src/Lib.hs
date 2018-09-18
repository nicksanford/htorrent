module Lib where

import qualified BEncode
import qualified FileManager
import qualified Shared
import Tracker (trackerRequest, toTracker)
import Utils (getPeerID)

import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.Chan (newChan, writeChan, Chan)

run :: String -> String -> Chan String -> IO ()
run filename port killChan = do
  peer_id <- getPeerID
  maybeBencode <- BEncode.maybeReadBencode filename
  let maybeTracker = maybeBencode >>= toTracker peer_id
  case maybeTracker of
    Right tracker -> do
      _ <- forkIO (FileManager.start tracker port killChan)
      return ()
    Left e ->
      print e
