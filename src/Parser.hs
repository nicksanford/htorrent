{-# LANGUAGE OverloadedStrings #-}
module Parser where

import qualified Data.Bits       as Bits
import qualified Data.ByteString as BS
import           Data.Foldable   (length, toList)
import qualified Data.Sequence   as Seq
import qualified Data.Word8      as W
import           Prelude         hiding (length)
import qualified Data.Attoparsec.ByteString as P
import Control.Applicative

import           Shared
import           Utils

defaultPeerRPCParse :: PeerRPCParse
defaultPeerRPCParse = PeerRPCParse Seq.empty Nothing []

keepAliveBS :: BS.ByteString
keepAliveBS = BS.pack [0,0,0,0]

chokeBS :: BS.ByteString
chokeBS = BS.pack [0,0,0,1,0]

unChokeBS :: BS.ByteString
unChokeBS = BS.pack [0,0,0,1,1]

interestedBS :: BS.ByteString
interestedBS = BS.pack [0,0,0,1,2]

notInterestedBS :: BS.ByteString
notInterestedBS = BS.pack [0,0,0,1,3]

haveBS :: BS.ByteString
haveBS = BS.pack [0,0,0,5,4]

cancelBS :: BS.ByteString
cancelBS = BS.pack [0,0,0,13,8]

blockRequestBS :: BS.ByteString
blockRequestBS = BS.pack [0,0,0,13,6]

keepAliveParser :: P.Parser PeerRPC
keepAliveParser = P.string keepAliveBS >> return PeerKeepAlive

chokeParser :: P.Parser PeerRPC
chokeParser = P.string chokeBS >> return Choke

unChokeParser :: P.Parser PeerRPC
unChokeParser = P.string unChokeBS >> return UnChoke

interestedParser :: P.Parser PeerRPC
interestedParser = P.string interestedBS >> return Interested

notInterestedParser :: P.Parser PeerRPC
notInterestedParser = P.string notInterestedBS >> return NotInterested

haveParser :: P.Parser PeerRPC
haveParser = P.string haveBS >> Have . partialToBigEndian <$> P.take 4

cancelParser :: P.Parser PeerRPC
cancelParser = let f = do
                        a <- P.take 4
                        b <- P.take 4
                        c <- P.take 4
                        return $ Cancel (partialToBigEndian a)
                                        (partialToBigEndian b)
                                        (partialToBigEndian c)
               in P.string cancelBS >> f

bitfieldParser :: P.Parser PeerRPC
bitfieldParser = do
  msgLen <- P.take 4
  let bitfieldLength = (partialToBigEndian $ msgLen) - 1
  _ <- P.word8 5
  rawBitfield <- P.take bitfieldLength
  let boolsBeforeCheck = BS.unpack rawBitfield >>= (\ x -> reverse [Bits.testBit x i | i <- [0 .. 7]])
  return $ BitField boolsBeforeCheck

blockResponseParser :: P.Parser PeerRPC
blockResponseParser = do
  msgLen <- P.take 4
  let blockLength = (partialToBigEndian $ msgLen) - 9
  _ <- P.word8 7
  index <- P.take 4
  begin <- P.take 4
  block <- P.take blockLength
  return $ Response $ BlockResponse (partialToBigEndian index)
                                    (partialToBigEndian begin)
                                    block

blockRequestParser :: P.Parser PeerRPC
blockRequestParser = do
  _ <- P.string blockRequestBS
  a <- P.take 4
  b <- P.take 4
  c <- P.take 4
  return $ Request $ BlockRequest (partialToBigEndian a)
                                  (partialToBigEndian b)
                                  (partialToBigEndian c)
                                  PeerInitiated
                                  0
                                  Nothing

rpcParser :: P.Parser PeerRPC
rpcParser =     keepAliveParser
            <|> chokeParser
            <|> unChokeParser
            <|> interestedParser
            <|> notInterestedParser
            <|> haveParser
            <|> cancelParser
            <|> blockResponseParser
            <|> bitfieldParser
            <|> blockRequestParser

rpcParse :: BS.ByteString -> BS.ByteString -> Either String (BS.ByteString, [PeerRPC])
rpcParse unparsed newBS  =
  go (unparsed <> newBS) Seq.empty
  where go xs rpcSequence =
          case P.parse rpcParser xs of
            P.Done rest result ->
              go rest (rpcSequence Seq.|> result)
            P.Fail _rest _strings string ->
              Left string
            P.Partial _ ->
              Right (xs, toList rpcSequence)


-- isValidBitField :: BS.ByteString -> Bool
-- isValidBitField bs = len == fromIntegral (length $ drop 5 xs)
--   where xs = BS.unpack bs
--         len = xs !! 3 - 1

-- maxIntInByteSize :: Int -> Integer
-- maxIntInByteSize byteSize = foldr (\_ acc -> 256 * acc) 1 [0..(byteSize-1)]  - 1
