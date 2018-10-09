{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import           Data.Foldable   (toList)
import           Control.Monad (forM_)

import Parser
import BEncode
import Tracker
import Shared

main :: IO ()
main = do
  eitherB <- maybeReadBencode "./arch-spec-0.3.pdf.torrent"
  case eitherB >>= toTracker "peer_id" of
    Right b -> do
      let pieceMap = [(h, False)| h <- tPieceHashes b]
      bs <- BS.readFile "./vcr/vcr.txt"
      let result = parseRPC pieceMap bs defaultPeerRPCParse
      forM_ (pRPCParsed result) print
    Left e ->
      print $ "ERROR: " <> e
