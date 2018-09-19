{-# LANGUAGE OverloadedStrings #-}

module Tracker ( toTracker
               , trackerRequest
               , load
               ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.UTF8  as UTF8
import           Data.List             (unfoldr)
import qualified Data.Map              as M
import qualified Network.HTTP.Simple   as HTTP
import           System.FilePath.Posix (joinPath, takeDirectory)

import           BEncode               (BEncode (..), Run (..),
                                        bencodeToMaybeDict,
                                        bencodeToMaybeInteger,
                                        bencodeToMaybeString, decode, encode,
                                        maybeReadBencode)
import           Shared
import           Utils                 (escape, getPeerID, shaHash)

-- TODO: Currently I am only dealing with single file info. I will add support for multi file info later.
toTracker :: BS.ByteString -> BEncode -> Either BS.ByteString Tracker
toTracker peerID (BDict d) =
  maybe (Left "ERROR: tracker invalid") Right $ buildTracker <$> maybeAnnounce
                                                             <*> maybePieceLength
                                                             <*> maybePieces
                                                             <*> maybeInfoHash
                                                             <*> singleFileInfo
  where buildTracker :: BS.ByteString -> Integer -> [BS.ByteString] -> BS.ByteString -> SingleFileInfo -> Tracker
        buildTracker announce pieceLength pieceHashes infoHash sfi =
          Tracker peerID announce pieceLength pieceHashes infoHash sfi directoryInfo maybeEncoding
        l :: BEncode -> Maybe BEncode
        l x = M.lookup (BString "info") d >>= bencodeToMaybeDict >>= M.lookup x
        maybeInfoHash :: Maybe BS.ByteString
        maybeInfoHash = shaHash . encode <$> M.lookup (BString "info") d
        maybeAnnounce :: Maybe BS.ByteString
        maybeAnnounce =  M.lookup (BString "announce") d >>= bencodeToMaybeString
        singleFileInfo :: Maybe SingleFileInfo
        singleFileInfo = SingleFileInfo <$> (l (BString "name") >>= bencodeToMaybeString)
                                        <*> (l (BString "length") >>= bencodeToMaybeInteger)
                                        <*> Just (l (BString "md5sum") >>= bencodeToMaybeString)
        directoryInfo :: Maybe DirectoryInfo
        directoryInfo = DirectoryInfo <$> (l (BString "name") >>= bencodeToMaybeString)
                                      <*> (l (BString "files") >>= bencodeToMaybeSingleFiles)
        maybeEncoding :: Maybe BS.ByteString
        maybeEncoding = M.lookup (BString "encoding") d >>= bencodeToMaybeString
        maybePieceLength :: Maybe Integer
        maybePieceLength = l (BString "piece length") >>= bencodeToMaybeInteger
        maybePieces :: Maybe [BS.ByteString]

        maybePieces = unfoldr extractPieces <$> (l (BString "pieces") >>= bencodeToMaybeString)
        extractPieces :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
        extractPieces bs =
          if BS.null bs
          then Nothing
          else Just (BS.take 20 bs, BS.drop 20 bs)
toTracker _ _ = Left "ERROR: Tracker invalid due to absence of both single file dict and directory dict"

createTrackerRequestPayload :: Tracker -> String -> Integer -> String
createTrackerRequestPayload t port downloaded =
  requestString
  where
        requestString = UTF8.toString $ BS.concat [ tAnnounce t
                                                  , "?peer_id=", tPeerId t
                                                  -- TODO: Have this passed in from the main thread if content has already been downloaded
                                                  , "&left=", UTF8.fromString $ show $ sfLength (tSingleFileInfo t) - downloaded
                                                  , "&event=started"
                                                  , BS.concat [ "&port=", UTF8.fromString port]
                                                  , "&uploaded=0"
                                                  , BS.concat [ "&downloaded=", UTF8.fromString $ show downloaded ]
                                                  , "&numwant=500"
                                                  , "&info_hash=", escape $ tInfoHash t
                                                  ]

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
      case M.lookup (BString "peers") d of
        (Just (BList l)) ->
          return $ TrackerResponse <$> traverse bDictToPeer l
                                   <*> (M.lookup (BString "interval") d >>= bencodeToMaybeInteger)
                                   <*> Just (M.lookup (BString "tracker id") d >>= bencodeToMaybeString)
                                   <*> Just (M.lookup (BString "warning message") d >>= bencodeToMaybeString)
                                   <*> Just (M.lookup (BString "min interval") d >>= bencodeToMaybeInteger)
                                   <*> Just (M.lookup (BString "complete") d >>= bencodeToMaybeString)
                                   <*> Just (M.lookup (BString "incomplete") d >>= bencodeToMaybeString)
        (Just (BString _)) ->
          --print "got string from peers" >>
          --print s >>
          return Nothing
        _ ->
          --print "ERROR: Got different value than expected from peers" >>
          --print other >>
          return Nothing
    _ ->
      --print "ERROR: Didn't parse correctly" >>
      --print decoded >>
      return Nothing

-- Convenience Functions
bDictToPeer :: BEncode -> Maybe Peer
bDictToPeer (BDict d) = Peer <$> (M.lookup (BString "ip") d >>= bencodeToMaybeString)
                             <*> (M.lookup (BString "port") d >>= bencodeToMaybeInteger)
bDictToPeer _ = Nothing

bencodeToMaybeSingleFiles :: BEncode -> Maybe [SingleFileInfo]
bencodeToMaybeSingleFiles (BList xs@(BDict _:_)) = traverse dictToMaybeSingleFile xs
bencodeToMaybeSingleFiles _ = Nothing

dictToMaybeSingleFile :: BEncode -> Maybe SingleFileInfo
dictToMaybeSingleFile (BDict x) = SingleFileInfo <$> (M.lookup (BString "path") x >>= bencodeToMaybeString) <*>
                                                       (M.lookup (BString "length") x >>= bencodeToMaybeInteger) <*>
                                                       Just (M.lookup (BString "md5sum") x >>= bencodeToMaybeString)
dictToMaybeSingleFile _ = Nothing

load :: String -> IO (Maybe Tracker)
load s = do
  peer_id <- getPeerID
  maybeBencode <- maybeReadBencode s
  -- TODO Refactor this using monad
  case maybeBencode of
    Right r ->
      case toTracker peer_id r of
        Right tracker -> do
          let sfi = tSingleFileInfo tracker
          let newSfi = sfi { sfName = UTF8.fromString $ joinPath [takeDirectory s, UTF8.toString $ sfName sfi] }
          return $ Just $ tracker {tSingleFileInfo =  newSfi}
        Left _ ->
          return Nothing
    Left _ ->
        return Nothing
