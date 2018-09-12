{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Maybe (fromJust, isJust)

import BEncode
import qualified Tracker as Tracker
import qualified FileManager as FM
import qualified Shared as Shared
import qualified Peer as Peer
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Word8 as W
import qualified Data.Sequence             as Seq

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
bigEndianToInteger_prop (FourByteBigEndian word8s) = word8s == Peer.integerToBigEndian (fromJust $ Peer.bigEndianToInteger word8s)
  

main :: IO ()
main = hspec $ do

  describe "peerRPCsToPieces" $ do
    it "should be able to retrieve all content in a file" $ do
      Just t <- Tracker.testTracker2 "./test/arch-spec-0.3.pdf.torrent"
      let pieceList = FM.getPieceList t
      let (Tracker.SingleFileInfo (Tracker.Name bsFileName) (Tracker.Length l) _) = Tracker.getTrackerSingleFileInfo t
      let fileName = UTF8.toString bsFileName
      fileContents <- BS.readFile fileName
      let pieceMap = fromJust $ FM.getCurrentPieceMap t fileContents
      (length pieceMap) `shouldBe` 54
      (filter (not . snd) pieceMap) `shouldBe` []
      length pieceList `shouldBe` 54
      let blockRequests = L.concat $ (\(Shared.Work _ xs) -> xs) <$> pieceList
      let requestLengthSum = sum $ (\(Shared.BlockRequest _ _ (Shared.RequestLength rl)) -> rl) <$> blockRequests
      requestLengthSum `shouldBe` fromIntegral (BS.length fileContents)
      let pieceLength = Tracker.getTrackerPieceLength t
      let requests = (\(Shared.BlockRequest (Shared.PieceIndex pieceIndex)
                                            (Shared.Begin b)
                                            (Shared.RequestLength rl)) -> Peer.Request pieceIndex b rl) <$> blockRequests
      pieces <- (Peer.peerRPCsToPieces pieceLength (bsFileName, l) requests)
      (BS.concat $ (\(Peer.Piece _ _ c) -> c) <$> pieces) `shouldBe` fileContents
      let piecesBS = BS.concat $ Peer.pieceToBS <$> pieces
      let emptySeq = Seq.empty
      let (Peer.PeerRPCParse buffer Nothing parsedPieces) = Peer.parseRPC (Peer.PieceMap $ fmap (\(a,b) -> (a,False)) pieceMap) piecesBS Peer.defaultPeerRPCParse
      buffer `shouldBe` Seq.empty
      length parsedPieces `shouldBe` length pieces
      (traverse Peer.peerRPCToPiece parsedPieces) `shouldBe` Just pieces

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
      Just tracker <- Tracker.testTracker2 "./test/example.torrent"
      let Tracker.SingleFileInfo (Tracker.Name _) (Tracker.Length totalLength) (Tracker.MD5Sum _) = Tracker.getTrackerSingleFileInfo tracker
      sum [len | Shared.BlockRequest _ _ (Shared.RequestLength len) <- FM.getRequestList tracker] `shouldBe` totalLength

      Just (Tracker.Tracker p a pl ps ih (Tracker.SingleFileInfo n (Tracker.Length l) md5) mdi me) <- Tracker.testTracker2 "./test/example.torrent"
      let newTracker = (Tracker.Tracker p a pl ps ih (Tracker.SingleFileInfo n (Tracker.Length (l+3)) md5) mdi me)
      sum [len | Shared.BlockRequest _ _ (Shared.RequestLength len) <- FM.getRequestList newTracker] `shouldBe` l+3

      let newnewTracker = (Tracker.Tracker p a pl ps ih (Tracker.SingleFileInfo n (Tracker.Length (l-3)) md5) mdi me)
      sum [len | Shared.BlockRequest _ _ (Shared.RequestLength len) <- FM.getRequestList newnewTracker] `shouldBe` l-3

    it "there should be no duplicate elements" $ do
      Just tracker <- Tracker.testTracker2 "./test/example.torrent"
      (S.size $ S.fromList $ FM.getRequestList tracker) `shouldBe` (fromIntegral $ length $ FM.getRequestList tracker)

    it "the length should never exceed the blockSize" $ do
      Just tracker <- Tracker.testTracker2 "./test/example.torrent"
      maximum [len | Shared.BlockRequest _ _ (Shared.RequestLength len) <- FM.getRequestList tracker] `shouldBe` Shared.blockSize

    it "the length should never be smaller or equal to 0" $ do
      Just tracker <- Tracker.testTracker2 "./test/example.torrent"
      minimum [len | Shared.BlockRequest _ _ (Shared.RequestLength len) <- FM.getRequestList tracker] `shouldSatisfy` (> 0)

    it "when grouped by pieceIndex, there should be the same number of pieces and the indexes should be the same as the piece indexes" $ do
      Just tracker <- Tracker.testTracker2 "./test/example.torrent"
      let pieces = Tracker.getTrackerPieces tracker
      let groupedRequestList = L.groupBy (\(Shared.BlockRequest (Shared.PieceIndex x) _ _) (Shared.BlockRequest (Shared.PieceIndex y) _ _) -> x == y) $ FM.getRequestList tracker
      length groupedRequestList `shouldBe` length pieces

    it "when grouped by pieceIndex, the indexes should be the same as the piece indexes" $ do
      Just tracker <- Tracker.testTracker2 "./test/example.torrent"
      let pieces = Tracker.getTrackerPieces tracker
      let rl = FM.getRequestList tracker
      let requestIndeciesSet = S.fromList $ fmap (\(Shared.BlockRequest (Shared.PieceIndex x) _ _) -> x) rl
      (S.size requestIndeciesSet) `shouldBe` (fromIntegral $ length pieces)

    it "still works if the total length is not a power of 2 above" $ do
      Just (Tracker.Tracker p a pl ps ih (Tracker.SingleFileInfo n (Tracker.Length l) md5) mdi me) <- Tracker.testTracker2 "./test/example.torrent"
      let newTracker = (Tracker.Tracker p a pl ps ih (Tracker.SingleFileInfo n (Tracker.Length (l+3)) md5) mdi me)
      let pieces = Tracker.getTrackerPieces newTracker
      let rl = FM.getRequestList newTracker
      let requestIndeciesSet = S.fromList $ fmap (\(Shared.BlockRequest (Shared.PieceIndex x) _ _) -> x) rl
      (S.size requestIndeciesSet) `shouldBe` (fromIntegral $ length pieces)

    it "still works if the total length is not a power of 2 below" $ do
      Just (Tracker.Tracker p a pl ps ih (Tracker.SingleFileInfo n (Tracker.Length l) md5) mdi me) <- Tracker.testTracker2 "./test/example.torrent"
      let newTracker = (Tracker.Tracker p a pl ps ih (Tracker.SingleFileInfo n (Tracker.Length (l-3)) md5) mdi me)
      let pieces = Tracker.getTrackerPieces newTracker
      let rl = FM.getRequestList newTracker
      let requestIndeciesSet = S.fromList $ fmap (\(Shared.BlockRequest (Shared.PieceIndex x) _ _) -> x) rl
      (S.size requestIndeciesSet) `shouldBe` (fromIntegral $ length pieces)
