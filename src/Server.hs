{-# LANGUAGE OverloadedStrings #-}
module Server (start) where

import           Control.Concurrent        (forkFinally)
import           Control.Concurrent.Chan   as Chan
import qualified Control.Exception         as E
import           Control.Monad             (forever, void, when)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as UTF8
import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)
import qualified System.Clock              as Clock

import           FSM
import qualified Peer
import           RPCMessages               (handshake, interested,
                                            readHandShake, unchoke,
                                            validateHandshake)
import           Shared
import           Tracker

start :: Opt -> Tracker -> Chan.Chan PieceRequest -> Chan.Chan ResponseMessage -> Chan.Chan a -> PieceMap -> IO ()
start opt tracker workC responseChan broadcastChan pieceMap = do
  putStrLn $ "BitTorrent TCP server running, and listening on port "  <> (show $ port opt)
  E.bracket (addrIO >>= open) close loop

  where hints = defaultHints { addrSocketType = Stream
                             , addrFlags = [AI_PASSIVE]
                             }
        addrIO = getAddrInfo (Just hints) Nothing (Just $ show $ port opt) >>= return . head
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          -- Solves issue withNetwork.Socket.bind: resource busy (Address already in use)
          setSocketOption sock ReuseAddr 1
          bind sock (addrAddress addr)
          setCloseOnExecIfNeeded $ fdSocket sock
          listen sock 10
          return sock

        loop sock = forever $ do
          (conn, peer) <- accept sock
          when (debug opt) $
            putStrLn $ "LOOP: Accepted connection " <> show conn
                                                    <> " from "
                                                    <> show peer
                                                    <> "\nBlocking until handshake is received"

          let threadEndHandler _ = when (debug opt) (putStrLn ("closing " <> show conn <> "  from " <> show peer))
                                   >> close conn
          void $ forkFinally (talk conn peer) threadEndHandler

        talk conn peer = do
          msg <- recv conn 68
          let maybeHandshakeResponse = readHandShake msg >>= validateHandshake tracker
          when (debug opt) $
            putStrLn $ "Peer " <> show peer
                              <> " sent handshake "
                              <> show maybeHandshakeResponse
                              <> "\nraw: "
                              <> UTF8.toString msg
                              <> " as the handshake"

          case maybeHandshakeResponse of
            Just peerResponse -> do
              let handshakeBS = handshake (tInfoHash tracker) (tPeerId tracker)
              sendAll conn handshakeBS
              let bf = pieceMapToBitField pieceMap
              time <- Clock.getTime Clock.Monotonic
              let fsmState = buildFSMState opt tracker (UTF8.fromString $ show peer) (prPeerId peerResponse) conn workC responseChan time pieceMap PeerInitiated
              fsmLog fsmState $ " got handshake: " <> show (BS.unpack bf)
                                                       <> " along with interested & unchoke messages "
              fsmLog fsmState $ " sending bitfield: " <> show (BS.unpack bf)
                                                          <> " along with interested & unchoke messages "
              sendAll conn bf
              sendAll conn interested
              sendAll conn unchoke
              let handleException e = fsmLog fsmState $ " HIT EXCEPTION " <> show (e :: E.SomeException)
              E.catch (recvLoop fsmState) handleException
            Nothing -> do
              when (debug opt) $
                putStrLn $ "Peer " <> show peer
                                  <> " got invalid handshake response "
                                  <> show maybeHandshakeResponse
              return ()
