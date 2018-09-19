{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module Peer where

import qualified Control.Concurrent.Chan                      as Chan
import           Control.DeepSeq
import qualified Control.Exception                            as E
import           Control.Monad                                (forM_, unless,
                                                               when)
import qualified Data.Binary                                  as Binary
import qualified Data.Bits                                    as Bits
import qualified Data.Bits.Bitwise                            as Bitwise
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Base16                       as B16
import qualified Data.ByteString.Lazy                         as Lazy
import qualified Data.ByteString.UTF8                         as UTF8
import qualified Data.Either                                  as Either
import           Data.Foldable                                (toList)
import           Data.List                                    (find, foldl',
                                                               sortOn)
import qualified Data.Map                                     as M
import           Data.Maybe                                   (fromJust,
                                                               fromMaybe,
                                                               isJust,
                                                               isNothing,
                                                               listToMaybe)
import qualified Data.Sequence                                as Seq
import qualified Data.Set                                     as S
import           FSM
import           Network.Socket                               hiding (recv)
import           Network.Socket.ByteString                    (recv, send,
                                                               sendAll)
import           RPCMessages                                  (initiateHandshake,
                                                               interested,
                                                               unchoke)
import           Shared
import qualified System.Clock                                 as Clock
import qualified System.Posix.Files.ByteString                as PosixFilesBS
import qualified System.Posix.IO                              as PosixIO
import qualified "unix-bytestring" System.Posix.IO.ByteString as PosixIOBS
import           System.Timeout                               (timeout)
import           Utils                                        (shaHashRaw,
                                                               unhex)

start :: Tracker -> Peer -> Chan.Chan PieceRequest -> Chan.Chan ResponseMessage -> Chan.Chan a -> PieceMap -> IO ()
start tracker peer workC responseChan broadcastC pieceMap =  do
  putStrLn $ "Initiating handshake with " <> show peer
  maybePeerResponse <- initiateHandshake tracker peer
  putStrLn $ "Handshake result " <> show maybePeerResponse <>  " with " <> show peer
  unless (isNothing maybePeerResponse) $ do
    let (peerResponse, conn) = fromJust maybePeerResponse
    let bitMap = pieceMapToBitField pieceMap
    putStrLn $ "Sending pieceMap to peer " <> show peer <> " bitmap: " <> show bitMap <> "\nas well as interested & unchoke"
    sendAll conn bitMap
    sendAll conn interested
    sendAll conn unchoke
    time <- Clock.getTime Clock.Monotonic
    let threadId = pIP peer <> (UTF8.fromString $ show $ pPort peer)
    let fsmState = buildFSMState tracker threadId (prPeerId peerResponse) conn workC responseChan time pieceMap SelfInitiated
    myLog fsmState $ " Starting recvLoop"
    E.catch (recvLoop fsmState) (\e -> do
                                    myLog fsmState $ " HIT EXCEPTION " <> (show (e :: E.SomeException))
                                    E.throw e
                                )
