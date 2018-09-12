{-# LANGUAGE OverloadedStrings #-}
module Tracker where

import Control.Monad (when)
import Data.List (unfoldr)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.Either (isRight)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as M
import qualified Network.HTTP.Simple as HTTP
import System.FilePath.Posix (takeDirectory, joinPath)

import BEncode ( BEncode (..)
               , Run (..)
               , bencodeToMaybeString
               , bencodeToMaybeDict
               , bencodeToMaybeInteger
               , decode
               , encode
               , maybeReadBencode
               )
import Shared
import Utils (escape, shaHash, getPeerID)

newtype Announce e = Announce e deriving (Eq, Show)
newtype Name e = Name e deriving (Eq, Show)
newtype Length e = Length e deriving (Eq, Show)
newtype Path e = Path e deriving (Eq, Show)
newtype MD5Sum e = MD5Sum e deriving (Eq, Show)
newtype Files e = Files e deriving (Eq, Show)
newtype Encoding e = Encoding e deriving (Eq, Show)
newtype PieceLength e = PieceLength e deriving (Eq, Show)
newtype Pieces e = Pieces e deriving (Eq, Show)
newtype InfoHash e = InfoHash e deriving (Eq, Show)
newtype PeerId e = PeerId e deriving (Eq, Show)
newtype Peers e = Peers e deriving (Eq, Show)
newtype TrackerId e = TrackerId e deriving (Eq, Show)
newtype Warning e = Warning e deriving (Eq, Show)
newtype Interval e = Interval e deriving (Eq, Show)
newtype MinInterval e = MinInterval e deriving (Eq, Show)
newtype Complete e = Complete e deriving (Eq, Show)
newtype InComplete e = InComplete e deriving (Eq, Show)

data SingleFileInfo = SingleFileInfo (Name BS.ByteString) (Length Integer) (MD5Sum (Maybe BS.ByteString)) deriving (Eq, Show)
data DirectoryFile = DirectoryFile (Path BS.ByteString) (Length Integer) (MD5Sum (Maybe BS.ByteString)) deriving (Eq, Show)
data DirectoryInfo = DirectoryInfo (Name BS.ByteString) (Files [DirectoryFile]) deriving (Eq, Show)

-- TODO: You will need to implement a Maybe PeerId here as it is possible for that to be provided. If it is, then you need to verify that the peer id you get back from the handshake is the same as what the tracker said.

data Tracker = Tracker (PeerId BS.ByteString) (Announce BS.ByteString) (PieceLength Integer) (Pieces [BS.ByteString]) (InfoHash BS.ByteString) (SingleFileInfo) (Maybe DirectoryInfo) (Maybe (Encoding BS.ByteString)) deriving (Eq, Show)

data TrackerResponse = TrackerResponse (Peers [Peer])
                                       (Maybe (TrackerId BS.ByteString))
                                       (Maybe (Warning BS.ByteString))
                                       (Interval Integer)
                                       (Maybe (MinInterval Integer))
                                       (Maybe (Complete BS.ByteString))
                                       (Maybe (InComplete BS.ByteString)) deriving (Eq, Show)

-- TODO: Currently I am only dealing with single file info. I will add support for multi file info later.
toTracker :: BS.ByteString -> BEncode -> Either BS.ByteString Tracker
toTracker peer_id (BDict d) =
  maybe (Left "ERROR: tracker invalid") Right $ buildTracker <$> (Just $ PeerId peer_id)
                                                             <*> maybeAnnounce
                                                             <*> maybePieceLength
                                                             <*> maybePieces
                                                             <*> maybeInfoHash
                                                             <*> singleFileInfo
                                                             -- >>= validateTracker
  where buildTracker :: PeerId BS.ByteString -> Announce BS.ByteString -> PieceLength Integer -> Pieces [BS.ByteString] -> InfoHash BS.ByteString -> SingleFileInfo -> Tracker
        buildTracker pid a pl p i sfi = Tracker pid a pl p i sfi directoryInfo maybeEncoding
        -- validateTracker :: Tracker -> Maybe Tracker
        -- validateTracker t@(Tracker _ _ _ _ _ sfi dfi _)
        --   | isNothing sfi && isNothing dfi = Nothing
        --   | otherwise = Just t
        l :: BEncode -> Maybe BEncode
        l x = M.lookup (BString "info") d >>= bencodeToMaybeDict >>= M.lookup x
        maybeInfoHash :: Maybe (InfoHash BS.ByteString)
        maybeInfoHash = InfoHash . shaHash . encode <$> M.lookup (BString "info") d
        maybeAnnounce :: Maybe (Announce BS.ByteString)
        maybeAnnounce =  Announce <$> (M.lookup (BString "announce") d >>= bencodeToMaybeString)
        singleFileInfo :: Maybe SingleFileInfo
        singleFileInfo = SingleFileInfo <$> (Name <$> (l (BString "name") >>= bencodeToMaybeString))
                                        <*> (Length <$> (l (BString "length") >>= bencodeToMaybeInteger))
                                        <*> Just (MD5Sum (l (BString "md5sum") >>= bencodeToMaybeString))
        directoryInfo :: Maybe DirectoryInfo
        directoryInfo = DirectoryInfo <$> (Name <$> (l (BString "name") >>= bencodeToMaybeString))
                                      <*> (Files <$> (l (BString "files") >>= bencodeToMaybeDirectoryFile))
        maybeEncoding :: Maybe (Encoding BS.ByteString)
        maybeEncoding = Encoding <$> (M.lookup (BString "encoding") d >>= bencodeToMaybeString)
        maybePieceLength :: Maybe (PieceLength Integer)
        maybePieceLength = PieceLength <$> (l (BString "piece length") >>= bencodeToMaybeInteger)
        maybePieces :: Maybe (Pieces [BS.ByteString])

        maybePieces = Pieces <$> unfoldr extractPieces <$> (l (BString "pieces") >>= bencodeToMaybeString)
        extractPieces :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
        extractPieces bs =
          if BS.null bs
          then Nothing
          else Just (BS.take 20 bs, BS.drop 20 bs)
toTracker _ _ = Left "ERROR: Tracker invalid due to absence of both single file dict and directory dict"

createTrackerRequestPayload (Tracker (PeerId peer_id) (Announce url) _ _ (InfoHash info_hash) singleFileInfo maybeDirectoryInfo _) port downloaded =
  requestString
  where
    -- left = if isJust maybeSingleFileInfo
    --           then  getSingleFileLength $ fromJust maybeSingleFileInfo
    --           else sum $ fmap getDirectoryFileLength $ getDirectoryInfoFiles $ fromJust maybeDirectoryInfo
        requestString = UTF8.toString $ BS.concat [ url
                                                  , "?peer_id=", peer_id
                                                  -- TODO: Have this passed in from the main thread if content has already been downloaded
                                                  , "&left=", UTF8.fromString $ show $ (getSingleFileLength singleFileInfo) - downloaded
                                                  , "&event=started"
                                                  , BS.concat [ "&port=", UTF8.fromString port]
                                                  , "&uploaded=0"
                                                  , BS.concat [ "&downloaded=", UTF8.fromString $ show downloaded ]
                                                  , "&numwant=500"
                                                  , "&info_hash=", escape info_hash
                                                  ]

-- TODO delete the host parameter
trackerRequest :: Tracker -> String -> Integer -> IO (Maybe TrackerResponse)
trackerRequest tracker port downloaded =
  HTTP.parseRequest (createTrackerRequestPayload tracker port downloaded) >>=
  HTTP.httpBS >>=
  handleTrackerRequest

handleTrackerRequest :: HTTP.Response BS.ByteString -> IO (Maybe TrackerResponse)
handleTrackerRequest response =
  case decode $ HTTP.getResponseBody response of
    (Run "" (Just (BDict d))) ->
      -- TODO refactor this case statement
      case (M.lookup (BString "peers") d) of
        (Just (BList l)) ->
          return $ TrackerResponse <$> fmap Peers (traverse bDictToPeer l)
                                   <*> Just (fmap TrackerId (M.lookup (BString "tracker id") d >>= bencodeToMaybeString))
                                   <*> Just (fmap Warning (M.lookup (BString "warning message") d >>= bencodeToMaybeString))
                                   <*> fmap Interval (M.lookup (BString "interval") d >>= bencodeToMaybeInteger)
                                   <*> Just (fmap MinInterval (M.lookup (BString "min interval") d >>= bencodeToMaybeInteger))
                                   <*> Just (fmap Complete (M.lookup (BString "complete") d >>= bencodeToMaybeString))
                                   <*> Just (fmap InComplete (M.lookup (BString "incomplete") d >>= bencodeToMaybeString))
        (Just (BString s)) ->
          --print "got string from peers" >>
          --print s >>
          return Nothing
        other ->
          --print "ERROR: Got different value than expected from peers" >>
          --print other >>
          return Nothing
    decoded ->
      --print "ERROR: Didn't parse correctly" >>
      --print decoded >>
      return Nothing

-- Convenience Functions
bDictToPeer :: BEncode -> Maybe Peer
bDictToPeer (BDict d) = Peer <$> (M.lookup (BString "ip") d >>= bencodeToMaybeString)
                             <*> (M.lookup (BString "port") d >>= bencodeToMaybeInteger)
bDictToPeer _ = Nothing

getSingleFileLength :: SingleFileInfo -> Integer
getSingleFileLength (SingleFileInfo _ (Length l) _) = l

getDirectoryFileLength :: DirectoryFile -> Integer
getDirectoryFileLength (DirectoryFile _ (Length l) _) = l

getDirectoryInfoFiles :: DirectoryInfo -> [DirectoryFile]
getDirectoryInfoFiles (DirectoryInfo _ (Files xs)) = xs

bencodeToMaybeDirectoryFile :: BEncode -> Maybe [DirectoryFile]
bencodeToMaybeDirectoryFile (BList xs@(BDict _:_)) = traverse dictToMaybeDirectoryFile xs
bencodeToMaybeDirectoryFile _ = Nothing


dictToMaybeDirectoryFile :: BEncode -> Maybe DirectoryFile
dictToMaybeDirectoryFile (BDict x) = DirectoryFile <$> (Path <$> (M.lookup (BString "path") x >>= bencodeToMaybeString)) <*>
                                                       (Length <$> (M.lookup (BString "length") x >>= bencodeToMaybeInteger)) <*>
                                                       Just (MD5Sum $  M.lookup (BString "md5sum") x >>= bencodeToMaybeString)
dictToMaybeDirectoryFile _ = Nothing

getTrackerInfoHash :: Tracker -> BS.ByteString
getTrackerInfoHash (Tracker _ _ _ _ (InfoHash bs) _ _  _) =  bs

getTrackerPieces :: Tracker -> [BS.ByteString]
getTrackerPieces (Tracker _ _ _ (Pieces bs) _ _ _  _) =  bs

getTrackerPieceLength :: Tracker -> Integer
getTrackerPieceLength (Tracker _ _ (PieceLength l) _ _ _ _  _) =  l

getTrackerSingleFileInfo :: Tracker -> SingleFileInfo
getTrackerSingleFileInfo (Tracker _ _ _ _ _ singleFileInfo _ _ ) = singleFileInfo

updateTrackerSingleFileInfo :: Tracker -> SingleFileInfo -> Tracker
updateTrackerSingleFileInfo (Tracker a b c d e _ f g) singleFileInfo = (Tracker a b c d e singleFileInfo f g)

getTrackerPeerId :: Tracker -> BS.ByteString
getTrackerPeerId (Tracker (PeerId peerId) _ _ _ _ _ _ _ ) = peerId

testTracker :: IO (Maybe Tracker)
testTracker = do
  peer_id <- getPeerID
  maybeBencode <- maybeReadBencode "./test/example.torrent"
  case maybeBencode of
    Right r ->
      case toTracker peer_id r of
        Right tracker ->
          return $ Just  tracker
        Left e2 ->
          return Nothing
    Left e ->
        return Nothing

testTracker2 :: String -> IO (Maybe Tracker)
testTracker2 s = do
  peer_id <- getPeerID
  maybeBencode <- maybeReadBencode s
  case maybeBencode of
    Right r ->
      case toTracker peer_id r of
        Right tracker -> do
          let (SingleFileInfo (Name bsFileName) b c) = getTrackerSingleFileInfo tracker
          return $ Just $ updateTrackerSingleFileInfo tracker (SingleFileInfo (Name $ UTF8.fromString $ joinPath [takeDirectory s,  UTF8.toString bsFileName]) b c)
        Left e2 ->
          return Nothing
    Left e ->
        return Nothing
