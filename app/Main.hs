{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent   (forkIO)

import qualified Data.ByteString      as BS
import qualified System.Environment   as SE
import System.Posix.Signals

import qualified Lib
import qualified Server
import System.IO
import Control.Monad
import Data.Char
import System.Exit
import Control.Concurrent.Chan (newChan, readChan, writeChan)

main :: IO ()
main = do
  args <- SE.getArgs
  killChan <- newChan
  let arg1:arg2:_ = args
  putStrLn "Press Ctrl-C to quit"
  _ <- forkIO $ Lib.run arg1 arg2 killChan
  _ <- installHandler keyboardSignal (Catch $ writeChan killChan "Quitting htorrent") Nothing
  exitOnQ killChan

exitOnQ killChan = do
    msg <- readChan killChan
    putStrLn msg
    exitSuccess
