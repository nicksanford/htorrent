{-# LANGUAGE OverloadedStrings #-}
module Parser ( defaultPeerRPCParse
              , parseRPC)
where

import qualified Data.Bits       as Bits
import qualified Data.ByteString as BS
import           Data.Foldable   (length, toList)
import           Data.Maybe      (fromJust)
import qualified Data.Sequence   as Seq
import qualified Data.Word8      as W
import           Prelude         hiding (length)

import           Shared
import           Utils

defaultPeerRPCParse :: PeerRPCParse
defaultPeerRPCParse = PeerRPCParse Seq.empty Nothing []

parseRPC :: PieceMap -> BS.ByteString -> PeerRPCParse -> PeerRPCParse
parseRPC pieceMap bs peerRPCParse =
  BS.foldl (parseRPC' pieceMap) peerRPCParse bs

parseRPC' :: PieceMap -> PeerRPCParse -> W.Word8 -> PeerRPCParse
parseRPC' pieceMap (PeerRPCParse word8Buffer Nothing xs) word8
  | newBuffer == Seq.fromList [0,0,0,0]   = PeerRPCParse Seq.empty Nothing (xs <> [PeerKeepAlive])
  | newBuffer == Seq.fromList [0,0,0,1,0] = PeerRPCParse Seq.empty Nothing (xs <> [Choke])
  | newBuffer == Seq.fromList [0,0,0,1,1] = PeerRPCParse Seq.empty Nothing (xs <> [UnChoke])
  | newBuffer == Seq.fromList [0,0,0,1,2] = PeerRPCParse Seq.empty Nothing (xs <> [Interested])
  | newBuffer == Seq.fromList [0,0,0,1,3] = PeerRPCParse Seq.empty Nothing (xs <> [NotInterested])
  | Seq.take 5 newBuffer == Seq.fromList [0,0,0,5,4] =
    if length newBuffer == 9 then
      let haveMsg = [Have $ BS.pack $ toList $ Seq.take 4 $ Seq.drop 5 newBuffer]
      in PeerRPCParse (Seq.drop 9 newBuffer) Nothing (xs <> haveMsg)
    else
      PeerRPCParse newBuffer Nothing xs
  | Seq.drop 4 (Seq.take 5 newBuffer) == Seq.singleton 5  = do
    let bitfieldLength = fromIntegral (fromJust $ bigEndianToInteger $ toList $ Seq.take 4 newBuffer) - 1
    let word8s = Seq.take bitfieldLength $ Seq.drop 5 newBuffer

    if ceiling ((fromIntegral . length $ pieceMap) / 8 :: Double) /= bitfieldLength then
      let e = Just "ERROR parseRPC in BitField parse, (ceiling ((fromIntegral . length $ T.getTrackerPieces tracker) / 8)) /= bitfieldLength"
      in PeerRPCParse newBuffer e xs
    else
      if length word8s /= bitfieldLength then
        PeerRPCParse newBuffer Nothing xs
      else do
        let boolsBeforeCheck = word8s >>= (\ x -> Seq.fromList $ reverse [Bits.testBit x i | i <- [0 .. 7]])
        let extraBits = Seq.drop (length pieceMap) boolsBeforeCheck

        -- If we have any extra bits we should  drop the connection
        if or extraBits then
          PeerRPCParse newBuffer (Just "ERROR parseRPC in BitField parse, extra bits are set") xs
        else
          let bitFieldMsg = [BitField $ zip (fmap fst pieceMap) (toList boolsBeforeCheck)]
          in PeerRPCParse (Seq.drop (bitfieldLength + 5) newBuffer) Nothing (xs <> bitFieldMsg)
  | Seq.take 5 newBuffer == Seq.fromList [0,0,0,13,6] =
        if length newBuffer >= 17 then
          let requestMsg = [Request $ BlockRequest (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 5 newBuffer)
                                                   (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 9 newBuffer)
                                                   (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 13 newBuffer)
                                                   PeerInitiated
                                                   0
                                                   Nothing]
          in PeerRPCParse Seq.empty Nothing (xs <> requestMsg)
        else
          PeerRPCParse newBuffer Nothing xs
  | Seq.drop 4 (Seq.take 5 newBuffer) == Seq.singleton 7 = do
    let blockLen = fromIntegral $ fromJust (bigEndianToInteger $ toList $ Seq.take 4 newBuffer) - 9
    let blockWord8s = Seq.take blockLen $ Seq.drop 13 newBuffer
    let indexWord8s = Seq.take 4 $ Seq.drop 5 newBuffer
    let beginWord8s = Seq.take 4 $ Seq.drop 9 newBuffer

    if length blockWord8s /= blockLen then
      PeerRPCParse newBuffer Nothing xs
    else
-- TODO refactor this with fmap
      let index = fromIntegral $ fromJust $ bigEndianToInteger $ toList indexWord8s
          begin = fromIntegral $ fromJust $ bigEndianToInteger $ toList beginWord8s
          block = BS.pack $ toList blockWord8s
          responseMsg = [Response $ BlockResponse index begin block]
      in PeerRPCParse (Seq.drop (5 + 4 + 4 + blockLen) newBuffer) Nothing (xs <> responseMsg)
  | Seq.take 5 newBuffer == Seq.fromList [0,0,0,13,8] =
    if length newBuffer >= 17 then
-- TODO refactor this with fmap
        let cancelMsg = [Cancel (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 5 newBuffer)
                                (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 9 newBuffer)
                                (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 13 newBuffer)]
        in PeerRPCParse Seq.empty Nothing (xs <> cancelMsg)
    else
        PeerRPCParse newBuffer Nothing xs
  | otherwise = PeerRPCParse newBuffer Nothing xs
  where newBuffer = word8Buffer Seq.|> word8

parseRPC' _ (PeerRPCParse word8Buffer e xs) word8 = PeerRPCParse newBuffer e xs
  where newBuffer = word8Buffer Seq.|> word8


-- isValidBitField :: BS.ByteString -> Bool
-- isValidBitField bs = len == fromIntegral (length $ drop 5 xs)
--   where xs = BS.unpack bs
--         len = xs !! 3 - 1

-- maxIntInByteSize :: Int -> Integer
-- maxIntInByteSize byteSize = foldr (\_ acc -> 256 * acc) 1 [0..(byteSize-1)]  - 1
