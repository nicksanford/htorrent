{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Maybe (isJust, fromJust, isNothing)
import qualified System.Directory as Dir
import qualified System.IO as SIO

import BEncode
import qualified Tracker as Tracker
import qualified FSM as FSM
import qualified FileManager as FM
import qualified Shared as Shared
import qualified Peer as Peer
import qualified Parser as Parser
import qualified Utils as Utils
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Word8 as W
import qualified Data.Sequence             as Seq
import qualified Data.List.NonEmpty as NonEmptyL
import Control.Monad (forM_)
import Utils (unhex, shaHashRaw, shaHash)

sizedBencode :: Int -> Gen BEncode
sizedBencode c
  | c <= 0 = do
    i <- arbitrary
    s <- fmap UTF8.fromString arbitrary
    elements [ BInteger i, BString s ]
  | c <= 10 = do
    ls <- vectorOf 2 $ sizedBencode (c - 1)
    ks <- vectorOf 2 (fmap UTF8.fromString arbitrary)
    elements [ BList ls
             , BDict (M.fromList $ zipWith (\k v -> (BString k, v)) ks ls) ]
  | otherwise = sizedBencode 10

genFourByteBigEndian :: Gen FourByteBigEndian
genFourByteBigEndian = FourByteBigEndian <$> (sequence [ arbitrary | _ <- [0..3]] )

instance Arbitrary BEncode where
  arbitrary = sized sizedBencode

instance Arbitrary FourByteBigEndian where
  arbitrary = genFourByteBigEndian

charsToMaybeInt_prop :: Positive Int -> Bool
charsToMaybeInt_prop (Positive x) = (charsToMaybeInt stringifiedXs) == (Just x)
  where stringifiedXs = UTF8.fromString $ show x

encodeDecodeRoundTrip_prop :: BEncode -> Bool
encodeDecodeRoundTrip_prop bencode = bencode == ((\(Run "" (Just x)) -> x) . decode . encode $ bencode)

newtype FourByteBigEndian = FourByteBigEndian [W.Word8] deriving (Eq, Show)

bigEndianToInteger_prop :: FourByteBigEndian -> Bool 
bigEndianToInteger_prop (FourByteBigEndian word8s) = word8s == Utils.integerToBigEndian (fromJust $ Utils.bigEndianToInteger word8s)

readBlock :: Shared.Tracker -> SIO.FilePath -> Shared.BlockRequest -> IO (BS.ByteString)
readBlock tracker filePath br =
  SIO.withBinaryFile filePath SIO.ReadMode f
  where f h = do
          SIO.hSeek h SIO.AbsoluteSeek ((Shared.tPieceLength tracker * (Shared.bIndex br)) + (Shared.bBegin br))
          BS.hGet h $ fromIntegral (Shared.bLength br)

main :: IO ()
main = hspec $ do

  describe "peerRPCsToPieces" $ do
    it "should be able to retrieve all content in a file" $ do
      Just t <- Tracker.load "./test/arch-spec-0.3.pdf.torrent"
      let pieceLength = Shared.tPieceLength t
      let pieceList = FM.getPieceList t
      let sfi = Shared.tSingleFileInfo t
      let l = Shared.sfLength sfi
      let bsFileName = Shared.sfName sfi
      let fileName = UTF8.toString bsFileName
      fileContents <- LBS.readFile fileName
      let pieceMap = FM.getCurrentPieceMap t fileContents
      (length pieceMap) `shouldBe` 54
      (filter (not . snd) pieceMap) `shouldBe` []
      length pieceList `shouldBe` 54
      let blockRequests = L.concat $ ((NonEmptyL.toList . Shared.preqBlockRequests) <$>  pieceList)
      let requestLengthSum = sum $ Shared.bLength <$> blockRequests
      requestLengthSum `shouldBe` fromIntegral (LBS.length fileContents)
      pieces <- (FSM.fetchBlockResponses pieceLength sfi (Shared.Request <$> blockRequests))
      (BS.concat $ Shared.pBlock <$> pieces) `shouldBe` (LBS.toStrict fileContents)
      let piecesBS = BS.concat $ FSM.blockResponseToBS <$> pieces
      let (Shared.PeerRPCParse buffer Nothing parsedBlockResponses) = Parser.parseRPC ((,False) . fst <$> pieceMap) piecesBS Parser.defaultPeerRPCParse
      buffer `shouldBe` Seq.empty
      length parsedBlockResponses `shouldBe` length pieces
      ((\(Shared.Response r) -> r) <$> parsedBlockResponses) `shouldBe` pieces

  describe "decode" $ do
    describe "charsToMaybeInt_prop" $ do
      it "it to have the round trip property" $ do
        quickCheck charsToMaybeInt_prop

    describe "strings" $ do
      it "can parse an empty string" $ do
        decode "0:" `shouldBe` Run  "" (Just (BString ""))
      it "returns a BString with no unparsed data if given a well formed string" $ do
        decode "5:hello" `shouldBe` Run  "" (Just (BString "hello"))
      it "returns a BString with no parsed data if given a malformed string" $ do
        decode "5:helloasdf" `shouldBe` Run "asdf" (Just (BString "hello"))

    describe "integers" $ do
      it "returns a BInteger with no unparsed data if given a well formed integer" $ do
        decode "i9001e" `shouldBe` Run "" (Just (BInteger 9001))
      it "returns a BInteger with parsed data if given a malformed integer" $ do
        decode "i9001easdf" `shouldBe` Run "asdf" (Just (BInteger 9001))
      it "returns a BInteger with no unparsed data if given a well formed negative integer" $ do
        decode "i-9001e" `shouldBe` Run "" (Just (BInteger (-9001)))

    describe "lists" $ do
      it "can parse an empty lists" $ do
        decode "le" `shouldBe` Run "" (Just (BList []))
      it "parses l4:spam4:eggse into BList [BString \"spam\", BString \"eggs\"]" $ do
        decode "l4:spam4:eggse" `shouldBe` Run "" (Just (BList [BString "spam", BString "eggs"]))
      it "parses invalid lists to nothing with the rest maintained" $ do
        decode "l4:spam4:eggsasdf" `shouldBe` Run "l4:spam4:eggsasdf" Nothing
      it "can parse li4ee" $ do
        decode "li4ee" `shouldBe` Run "" (Just $ BList [BInteger 4])

    describe "dicts" $ do
      it "can parse an empty dict" $ do
        decode "de" `shouldBe` Run "" (Just (BDict $ M.fromList []))
      it "can parse a dict" $ do
        decode "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee" `shouldBe` Run "" (Just (BDict $ M.fromList $ [(BString "publisher",BString "bob"), (BString "publisher-webpage",BString "www.example.com"),(BString "publisher.location",BString "home")]))
      it "parses d3:cow3:moo4:spam4:eggse into BDict M.map [(\"cow\",\"moo\"), (\"spam\", \"eggs\")]" $ do
        decode "d3:cow3:moo4:spam4:eggse" `shouldBe` Run "" (Just $ BDict $ M.fromList [(BString "cow", BString "moo"), (BString "spam", BString "eggs")])
      it "parses lists in dicts" $ do
        decode "d4:spaml1:a1:bee" `shouldBe` Run "" (Just $ BDict $ M.fromList [(BString "spam", BList [BString "a", BString "b"])])



  describe "encode" $ do
    it "has the round-trip property with decode" $ do
        quickCheck encodeDecodeRoundTrip_prop

  describe "bigEndianToInteger" $ do
    it "should have the round trip property with integerToBigEndian" $ do
      quickCheck bigEndianToInteger_prop

  describe "getRequestList" $ do
    it "when all lengths are summed up it should equal the length of the content" $ do
      Just tracker <- Tracker.load "./test/example.torrent"
      let sfi = Shared.tSingleFileInfo tracker
      let totalLength= Shared.sfLength sfi
      sum [Shared.bLength br | br <- FM.getRequestList tracker] `shouldBe` totalLength

      let newTracker = tracker { Shared.tSingleFileInfo = sfi { Shared.sfLength = (3 + totalLength)} }
      sum [Shared.bLength br | br <- FM.getRequestList newTracker] `shouldBe` totalLength+3

      let newnewTracker = tracker { Shared.tSingleFileInfo = sfi { Shared.sfLength = (totalLength - 3)} }
      sum [Shared.bLength br | br <- FM.getRequestList newnewTracker] `shouldBe` (totalLength-3)

    it "there should be no duplicate elements" $ do
      Just tracker <- Tracker.load "./test/example.torrent"
      (S.size $ S.fromList $ FM.getRequestList tracker) `shouldBe` (fromIntegral $ length $ FM.getRequestList tracker)

    it "the length should never exceed the blockSize" $ do
      Just tracker <- Tracker.load "./test/example.torrent"
      maximum [Shared.bLength br | br <- FM.getRequestList tracker] `shouldBe` Shared.blockSize

    it "the length should never be smaller or equal to 0" $ do
      Just tracker <- Tracker.load "./test/example.torrent"
      minimum [Shared.bLength br | br <- FM.getRequestList tracker] `shouldSatisfy` (> 0)

    it "when grouped by pieceIndex, there should be the same number of pieces and the indexes should be the same as the piece indexes" $ do
      Just tracker <- Tracker.load "./test/example.torrent"
      let pieces = Shared.tPieceHashes tracker
      let groupedRequestList = L.groupBy (\brx bry -> Shared.bIndex brx == Shared.bIndex bry) $ FM.getRequestList tracker
      length groupedRequestList `shouldBe` length pieces

    it "when grouped by pieceIndex, the indexes should be the same as the piece indexes" $ do
      Just tracker <- Tracker.load "./test/example.torrent"
      let pieces = Shared.tPieceHashes tracker
      let rl = FM.getRequestList tracker
      let requestIndeciesSet = S.fromList $ fmap Shared.bIndex rl
      (S.size requestIndeciesSet) `shouldBe` (fromIntegral $ length pieces)

    it "still works if the total length is not a power of 2 above" $ do
      Just tracker <- Tracker.load "./test/example.torrent"
      let sfi = Shared.tSingleFileInfo tracker
      let newTracker = tracker { Shared.tSingleFileInfo = sfi { Shared.sfLength = (3 + Shared.sfLength sfi)} }
      let pieces = Shared.tPieceHashes newTracker
      let rl = FM.getRequestList newTracker
      let requestIndeciesSet = S.fromList $ fmap Shared.bIndex rl
      (S.size requestIndeciesSet) `shouldBe` (fromIntegral $ length pieces)

    it "still works if the total length is not a power of 2 below" $ do
      Just tracker <- Tracker.load "./test/example.torrent"
      let sfi = Shared.tSingleFileInfo tracker
      let newTracker = tracker { Shared.tSingleFileInfo = sfi { Shared.sfLength = (Shared.sfLength sfi - 3)} }
      let pieces = Shared.tPieceHashes newTracker
      let rl = FM.getRequestList newTracker
      let requestIndeciesSet = S.fromList $ fmap Shared.bIndex rl
      (S.size requestIndeciesSet) `shouldBe` (fromIntegral $ length pieces)
