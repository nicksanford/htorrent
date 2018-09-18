{-# LANGUAGE OverloadedStrings #-}
module RPCMessages where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Word8 as W
import Data.Maybe (listToMaybe)
import Shared
import Utils

keepAlive :: BS.ByteString
keepAlive = BS.pack [0,0,0,0]

choke :: BS.ByteString
choke = BS.pack [0,0,0,1,0]

unchoke :: BS.ByteString
unchoke = BS.pack [0,0,0,1,1]

interested :: BS.ByteString
interested = BS.pack [0,0,0,1,2]

notInterested :: BS.ByteString
notInterested = BS.pack [0,0,0,1,3]

have :: BS.ByteString -> BS.ByteString
have pieceIndex = BS.concat [BS.pack [0,0,0,5,4], pieceIndex]

request :: Integer -> Integer -> Integer -> BS.ByteString
request index begin len =  BS.pack  $ [0,0,0,13,6] <> ([index, begin, len] >>= (integerToBigEndian . fromIntegral))

handshake :: BS.ByteString -> BS.ByteString -> BS.ByteString
handshake info_hash peer_id =  BS.concat [pstrlen, pstr, reserved, unhex info_hash, peer_id]
  where pstr = "BitTorrent protocol"
        pstrlen = BS.singleton $ fromIntegral (BS.length pstr)
        reserved = BS.replicate 8 W._nul

initiateHandshake :: Tracker -> Shared.Peer -> IO (Maybe (PeerResponse, Socket))
initiateHandshake t peer = do
  (response, conn) <- sendHandshake peer $ handshake (tInfoHash t) (tPeerId t)
  let validated = readHandShake response >>= validateHandshake t
  case validated of
    Nothing -> do
      close conn
      return Nothing
    Just x ->
      return $ Just (x, conn)

readHandShake :: BS.ByteString  ->  Maybe PeerResponse
readHandShake r = PeerResponse <$> maybeInfoHash <*> maybePeerId
  where word8s = BS.unpack r
        afterProtocol :: Maybe BS.ByteString
        afterProtocol = BS.pack . flip drop word8s . (1+) . fromIntegral <$> listToMaybe word8s
        afterEmpty :: Maybe BS.ByteString
        afterEmpty = BS.drop 8 <$> afterProtocol
        maybeInfoHash :: Maybe BS.ByteString
        maybeInfoHash = BS.take 20 <$> afterEmpty
        maybePeerId :: Maybe BS.ByteString
        maybePeerId = BS.drop 20 <$> afterEmpty

validateHandshake :: Tracker -> PeerResponse -> Maybe PeerResponse
validateHandshake t pr =
                  if unhex (tInfoHash t) == prInfoHash pr && tPeerId t /= prPeerId pr then
                    Just pr
                  else
                    Nothing

sendHandshake :: Shared.Peer -> BS.ByteString -> IO (BS.ByteString, Socket)
sendHandshake (Shared.Peer ip port) bs = do
  sock <- addrIO >>= open
  sendAll sock bs
  -- This is to make it so that I don't overfetch here, given that I know
  -- exactly how many bytes I need to read in for the handshake response.
  -- 49 + (length "BitTorrent protocol") == 58
  msg <- recv sock 68
  return (msg, sock)

  where hints = defaultHints { addrSocketType = Stream }
        addrIO = head <$> getAddrInfo (Just hints) (Just $ UTF8.toString ip) (Just $ show port)
        open addr = do
          -- TODO Consider setting recv timeout https://hackage.haskell.org/package/network-2.7.0.2/docs/Network-Socket.html 
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          connect sock $ addrAddress addr
          return sock
