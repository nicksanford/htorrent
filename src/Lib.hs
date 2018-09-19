module Lib where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Options.Applicative
import           System.Exit (exitSuccess)
import           System.Posix.Signals
import           Tracker                 (toTracker)
import           Utils                   (getPeerID)
import qualified BEncode
import qualified Control.Concurrent.Chan as Chan
import qualified FileManager
import Shared

exec :: IO ()
exec = do
  opt <- execParser opts
  killChan <- newChan
  putStrLn "Press Ctrl-C to quit"
  _ <- forkIO $ run opt killChan
  _ <- installHandler keyboardSignal (Catch $ writeChan killChan "Quitting htorrent") Nothing
  exitOnQ killChan

run :: Opt -> Chan String -> IO ()
run opt killChan = do
  peer_id <- getPeerID
  maybeBencode <- BEncode.maybeReadBencode (tracker opt)
  let maybeTracker = maybeBencode >>= toTracker peer_id
  case maybeTracker of
    Right tracker ->
      FileManager.start tracker opt killChan
    Left e ->
      print e

optParser :: Parser Opt
optParser = Opt <$> strOption (  long "tracker"
                              <> short 't'
                              <> metavar "TRACKER_FILE"
                              <> help "Path to the tracker file"
                              )
                <*> switch (  long "debug"
                           <> short 'd'
                           <> help "Enables debug information to stdout"
                           )
                <*> option auto (  long "port"
                                <> help "The port to have the TCP server listen on"
                                <> showDefault
                                <> value 8888
                                <> metavar "PORT"
                                )

opts :: ParserInfo Opt
opts = info (optParser <**> helper)
            (  fullDesc
            <> progDesc "HTorrent is a work-in-progress BitTorrent client written in Haskell."
            )

exitOnQ :: Chan.Chan String -> IO ()
exitOnQ killChan = do
    msg <- readChan killChan
    putStrLn msg
    exitSuccess
