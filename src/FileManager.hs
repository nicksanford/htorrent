{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module FileManager where
import Tracker
import Shared
import qualified Server
import Utils (unhex, shaHashRaw, shaHash)
import qualified System.IO as SIO
import qualified Control.Concurrent.Chan as Chan
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, isNothing)
import qualified System.IO as SIO
import qualified System.Directory as Dir
import qualified Peer as Peer
import Data.Foldable (forM_)
import System.Exit (exitSuccess)
import Control.Monad (when, unless)
import Control.Concurrent (forkFinally, forkIO, ThreadId, threadDelay)
import qualified System.Clock as Clock
import qualified Control.Concurrent.STM.TChan as TChan
import qualified System.Posix.IO as PosixIO
import qualified "unix-bytestring" System.Posix.IO.ByteString as PosixIOBS
import qualified System.Posix.Files.ByteString as PosixFilesBS
import qualified Control.Exception as E


-- data Tracker = Tracker (PeerId BS.ByteString)
--                        (Announce BS.ByteString)
--                        (PieceLength Integer)
--                        (Pieces [BS.ByteString])
--                        (InfoHash BS.ByteString)
--                        (SingleFileInfo)
--                        (Maybe DirectoryInfo)
--                        (Maybe (Encoding BS.ByteString))
--              deriving (Eq, Show)
-- data TrackerResponse = TrackerResponse (Peers [Peer])
--                                        (Maybe (TrackerId BS.ByteString))
--                                        (Maybe (Warning BS.ByteString))
--                                        (Interval Integer)
--                                        (Maybe (MinInterval Integer))
--                                        (Maybe (Complete BS.ByteString))
--                                        (Maybe (InComplete BS.ByteString)) deriving (Eq, Show)

-- data FileManagerState = FileManagerState (PeerId BS.ByteString)
--                                          (PieceLength Integer)
--                                          (Pieces Integer)
--                                          (BlockSize Integer) -- will set this to 16k
--                                          (DownloadedPieces (M.Map BS.ByteString (M.Map )))

--blockSize = 2^14 -- 16k
-- TODO: I am going to start off with only implementing this for a single file, in multifile mode I will need to maintain multiple files on the filesystem, I will deal with that once I have gotten single file downloads & uploads working.

getDefaultPieceMap :: Tracker -> [(BS.ByteString, Bool)]
getDefaultPieceMap tracker = (\piece -> (piece, False)) <$> getTrackerPieces tracker

getFileHashes :: Tracker -> LBS.ByteString -> Maybe [BS.ByteString]
getFileHashes tracker fileContents = do
  let singleFileInfo@(Tracker.SingleFileInfo _ (Tracker.Length fileLength) _) = getTrackerSingleFileInfo tracker
  if (fromIntegral $ LBS.length fileContents) /= fileLength then
    Nothing
  else
    Just $ L.unfoldr f fileContents
  where f x =
          if LBS.null x then
            Nothing
          else
            Just (shaHashRaw (LBS.toStrict $ LBS.take pieceLength x), LBS.drop pieceLength x)
        pieceLength = fromIntegral $ getTrackerPieceLength tracker

getCurrentPieceMap :: Tracker -> LBS.ByteString -> Maybe [(BS.ByteString, Bool)]
getCurrentPieceMap tracker fileContent = do
  let pm = getDefaultPieceMap tracker
  let fh = getFileHashes tracker fileContent
  (\bs -> zipWith (\b  (k,v) -> if b == k then (k, True) else (k, False)) bs pm) <$> fh

getRequestList :: Tracker -> [BlockRequest]
getRequestList tracker = do
  let pieces :: [BS.ByteString ]
      pieces = getTrackerPieces tracker
  let pieceLength = getTrackerPieceLength tracker
  let xs = [(b,min blockSize (pieceLength - b)) | b <- takeWhile (<pieceLength) $ iterate (+blockSize) 0]
  let ys = [BlockRequest (PieceIndex p) (Begin b) (RequestLength s)
           | p <- [0..fromIntegral $ (length pieces) - 2], (b, s) <- xs]
  let SingleFileInfo (Name _) (Length totalLength) (MD5Sum _) = getTrackerSingleFileInfo tracker
  let remainingLength = totalLength - pieceLength * (fromIntegral $ length pieces - 1)
  let lastPieceIndex = fromIntegral (length pieces) - 1
  let xxs = [BlockRequest (PieceIndex lastPieceIndex) (Begin b) (RequestLength $ min blockSize (remainingLength - b))
            | b <- takeWhile (<remainingLength) $ iterate (+blockSize) 0]
  ys ++ xxs

getPieceList :: Tracker.Tracker -> [WorkMessage]
getPieceList tracker = (\(w@((BlockRequest (PieceIndex i) _ _ ):_)) -> Work (PieceIndex i) w) <$> (L.groupBy (\(BlockRequest (PieceIndex x) _ _ ) (BlockRequest (PieceIndex y) _ _ ) -> x == y) $ getRequestList tracker)

responseToMaybeRequest :: ResponseMessage -> Maybe WorkMessage
responseToMaybeRequest (Failed xs) = Just xs
responseToMaybeRequest _ = Nothing


loop tracker workChan responseChan peers killChan pieceMap checkouts filteredWorkToBeDone = do
  -- print $ "RESPONSE CHANNEL: number of active peers: " ++ (show $ length peers) ++ " peers " ++ (show peers)
  response <- Chan.readChan responseChan
  --print $ "RESPONSE CHANNEL got data"
  let Tracker.SingleFileInfo (Tracker.Name bsFileName) (Tracker.Length fileLength) _ = getTrackerSingleFileInfo tracker
  let fileName = UTF8.toString bsFileName

  (newPeers, newPieceMap, newCheckouts) <- case response of
    (Succeeded (PieceResponse (PieceIndex i) (PieceContent c))) -> do
      -- let hash = UTF8.toString $ shaHash c
      -- let hashFilePath = (fileName ++ "dir/" ++ hash)
      -- BS.writeFile hashFilePath c
      --print $ "RESPONSE CHANNEL WROTE " ++ hashFilePath
      writePiece tracker fileName (PieceResponse (PieceIndex i) (PieceContent c))
      --print $ "RESPONSE CHANNEL: removing piece " ++ (show i) ++ " from checkouts"
      let newCo = M.delete i checkouts
      --print $ "RESPONSE CHANNEL: new checkouts " ++ (show newCo)
      let newPM = (\(pieceIndex, (k,v)) -> if pieceIndex == i then (k,True) else (k,v)) <$> zip [0..] pieceMap
      return (peers, newPM, newCo)
    (Error p) -> do
      let n = filter (/=p) peers
      --print $ "RESPONSE CHANNEL: Peer " ++ show p ++ " is no longer running. new peer list: " ++ show n
      return (n, pieceMap, checkouts)
    co@(CheckOut (PeerThreadId peerThreadId) (PieceIndex i) timestmap) ->
      if M.member i checkouts then do
        --print $ "ERROR: Got checkout message for work already checkedout this should be impossible " ++ (show co)
        Chan.writeChan killChan ()
        return (peers, pieceMap, checkouts)
      else do
        let new = M.insert i (peerThreadId, timestmap) checkouts
        -- print $ "RESPONSE CHANNEL: adding checkout " ++ (show co)
        -- print $ "RESPONSE CHANNEL: new checkouts " ++ (show new)
        return (peers, pieceMap, new)
    hb@(HeartBeat _ (PieceIndex i)) -> do
      let look = M.lookup i checkouts

      if isNothing look then do
        -- print $ "ERROR: Got heartbeat message for something not checked out, this should be impossible " ++ (show $  hb)
        Chan.writeChan killChan ()
        return (peers, pieceMap, checkouts)
      else do
        time <- Clock.getTime Clock.Monotonic
        let newCo = M.adjust (\(peerThreadId, _) -> (peerThreadId, time)) i checkouts
        -- print $ "RESPONSE CHANNEL: Got heartbeat: " ++ (show hb) ++ " updated checkouts: " ++ (show newCo)
        return (peers, pieceMap, newCo)
    (Failed f) -> do
      -- print $ "RESPONSE CHANNEL: hit failure " ++ (show f)
      return (peers, pieceMap, checkouts)
    CheckWork -> do
        time <- Clock.getTime Clock.Monotonic
-- If it has been more than 5 seconds since a heartbeat, assume the thread is dead and recreate the work.
        let f = (\k (peerThreadId, heartbeatTimeSpec) (newChenckouts, workToBeReDone) ->
                   if 5 > (Clock.sec $ Clock.diffTimeSpec time heartbeatTimeSpec) then
                     (M.delete k newChenckouts, (k, peerThreadId):workToBeReDone)
                   else
                     (newChenckouts, workToBeReDone)
                )
        let (newCo, newWork)= M.foldrWithKey f (checkouts, []) checkouts
        -- print $ "RESPONSE CHANNEL: Dead indexes & peers " ++ (show newWork)
        let newWorkIndxs :: [Integer]
            (newWorkIndxs, _) = unzip newWork
        let regeneratedWork = filter (\(Work (PieceIndex i) _) -> i `elem` newWorkIndxs) filteredWorkToBeDone
        Chan.writeList2Chan workChan regeneratedWork
        -- print $ "RESPONSE CHANNEL: Regenerated work " ++ (show regeneratedWork)
        _ <- forkIO $ checkoutTimer responseChan
        return (peers, pieceMap, newCo)
    CheckPeers ->
        return (peers, pieceMap, checkouts)

  case responseToMaybeRequest response of
    (Just work) ->
      Chan.writeChan workChan work
    _ ->
      return ()

  print $ "Downloaded " ++ (show $ floor ((((fromIntegral $ length (filter (snd) newPieceMap)) / (fromIntegral $ length newPieceMap))) * 100)) ++ "%"
  when (allPiecesWritten newPieceMap) $ do
    print "DONE!"
    -- Chan.writeChan killChan ()

  loop tracker workChan responseChan newPeers killChan newPieceMap newCheckouts filteredWorkToBeDone
  where allPiecesWritten = all snd

secToNanoSec :: Integer -> Integer
secToNanoSec = (*) 1000000000

nanoSectoSec :: Integer -> Integer
nanoSectoSec = (flip div) 1000000000

setupFilesAndCreatePieceMap :: Tracker -> Chan.Chan () -> IO [(BS.ByteString, Bool)]
setupFilesAndCreatePieceMap tracker killChan =  do
  let singleFileInfo@(Tracker.SingleFileInfo (Tracker.Name bsFileName) _ _) = getTrackerSingleFileInfo tracker
  let fileName = UTF8.toString bsFileName
  fileExists <- Dir.doesFileExist fileName
  unless fileExists $ do
    createFile singleFileInfo

  fileContents <- LBS.readFile fileName
  let maybePieceMap = getCurrentPieceMap tracker fileContents

  when (isNothing maybePieceMap) $ do
    createFile singleFileInfo

  fileContents2 <- LBS.readFile fileName
  let maybePieceMap2 = getCurrentPieceMap tracker fileContents2

  when (isNothing maybePieceMap2) $ do
    -- print "tracker is corrupt"
    Chan.writeChan killChan ()


  return $ fromJust maybePieceMap2

downloadedSoFar :: Tracker.Tracker -> [(BS.ByteString, Bool)] -> Integer
downloadedSoFar tracker pieceMap = do
  let singleFileInfo = getTrackerSingleFileInfo tracker

  if and $ fmap snd pieceMap then
    getSingleFileLength singleFileInfo
  else
    (fromIntegral $ length $ filter snd pieceMap) * getTrackerPieceLength tracker

forkPeer :: Tracker.Tracker -> Chan.Chan WorkMessage -> Chan.Chan ResponseMessage -> Chan.Chan a -> Peer.PieceMap -> Peer  -> IO ThreadId
forkPeer tracker workChan responseChan broadcastChan pieceMap peer =
  forkFinally (Peer.start tracker peer workChan responseChan broadcastChan pieceMap) (errorHandler peer responseChan)

errorHandler :: (Show a, Show b) => Peer -> Chan.Chan ResponseMessage -> Either a b -> IO ()
errorHandler peer chan (Left x) = do
  Chan.writeChan chan $ Error peer
  putStrLn $ "FORK FINALLY HIT ERROR ON START PEER: " ++ show peer ++ " " ++ show x
errorHandler peer chan (Right x) = do
  Chan.writeChan chan $ Error peer
  putStrLn $ "FORK FINALLY SUCCEEDED: " ++ show peer ++ " " ++ show x

getPeers :: TrackerResponse -> [Peer]
getPeers (TrackerResponse (Peers peers) _ _ _ _ _ _) = peers

secondsBetweenTrackerCalls :: TrackerResponse -> Int
secondsBetweenTrackerCalls (TrackerResponse _ _ _ (Interval interval) _ _ _) = fromIntegral interval

start :: Tracker.Tracker -> String -> Chan.Chan () -> IO ()
start tracker port killChan = do

  --putStrLn $ "ADDING WORK TO CHANNEL: " ++ (show workToBeDone)
  -- TODO: generate the current file state
  -- TODO: if file state is invlaid (bytes are written to the piece but it doesn't match the hash) then log it and create a new file.
  -- TODO: I think all peers need to know the current file sate so that they can send bitfield / have messages
  -- TODO: THOUGHT: Maybe they can just make a request to the tracker on a 1:1 basis for what the current bitfield should be (given that a biefield should only be sent when a peer connects) - maybe a biefield is only for the 'server' to send???
  -- TODO: I think that the simplest, crappyest way to do this is to just keep the file if it exists, and filter out the work messages which are already done. Bitfield generation can happen later. As can detecting errors in the file.
  -- TODO: I need some way to track bandwidth to prefer highr bandwdth peers
  pieceMap <- setupFilesAndCreatePieceMap tracker killChan
  let piecesLeft = filter (not . snd) pieceMap

  workChan <- Chan.newChan
  responseChan <- Chan.newChan
  broadcastChan <- Chan.newChan
  -- start server here
  _ <- forkIO $ Server.start port tracker workChan responseChan broadcastChan (Peer.PieceMap pieceMap)

  maybeTrackerResponse <- trackerRequest tracker port (downloadedSoFar tracker pieceMap)
  -- print ("maybeTrackerResponse: " ++ show maybeTrackerResponse)
  when (isNothing maybeTrackerResponse) $ do
    -- print "ERROR: got empty tracker response"
    Chan.writeChan killChan ()

  let trackerResponse = fromJust maybeTrackerResponse
  let peers = getPeers trackerResponse

  putStrLn "spawning child threads for peers"
  mapM_ (forkPeer tracker workChan responseChan broadcastChan (Peer.PieceMap pieceMap)) peers
  --_ <- forkIO $ timer tracker port  (secondsBetweenTrackerCalls trackerResponse)
  _ <- forkIO $ checkoutTimer responseChan
     -- number of microseconds in a second

  let workToBeDone :: [WorkMessage]
      workToBeDone = getPieceList tracker
  let filteredWorkToBeDone = fst <$> filter (not . snd . snd) (zip workToBeDone pieceMap)
  -- print $ "fileHashes: " ++ (show $ take 5 <$> getFileHashes tracker fileContents)
  -- print $ "pieceMap " ++ (show $ take 5 <$> pieceMap)
  -- print $ "workToBeDone " ++ (show $ fmap (\(Work xs)-> head xs) $ take 5 workToBeDone)
  -- print $ "filteredPieceList length : " ++ (show $ length filteredPieceList)
  -- print $ "filteredPieceList " ++ show filteredPieceList
  Chan.writeList2Chan workChan filteredWorkToBeDone
  loop tracker workChan responseChan peers killChan pieceMap M.empty filteredWorkToBeDone

-- TODO: [TODO] -> [Doing]
-- TODO: Have each peer thread send a message once they have checked out work
-- TODO: Peers must send heartbeats (once a second) that they are still working on the work to the parent.
-- TODO: If the parent fails to get heartbeats from peers for 5 seconds, then it assumes the peer is dead and enqueues the work again.
-- TODO: Once work is done it can no longer be checked out
-- timer tracker pieceMap port waitTime broadcastChan workChan responseChan = do
--   threadDelay (waitTime * 1000000)
--   broadCastState <- Chan.getChanContents broadcastChan 
--   let newPiecemap = mergeHavesIntoPieceMap pieceMap broadCastState
--   currentState <- Chan.getChanContents workChan 
--   if length currentState <
--   --Chan.writeList2Chan
--   maybeTrackerResponse <- trackerRequest tracker port (downloadedSoFar tracker pieceMap)
--   let newWaitTime = maybe waitTime secondsBetweenTrackerCalls maybeTrackerResponse
--   timer tracker port newWaitTime workChan responseChan

checkoutTimer :: Chan.Chan ResponseMessage -> IO ()
checkoutTimer responseChan = do
  threadDelay 1000000
  Chan.writeChan responseChan CheckWork

createFile :: Tracker.SingleFileInfo -> IO ()
createFile (Tracker.SingleFileInfo (Tracker.Name fileName) (Tracker.Length fileLength) _) = SIO.withBinaryFile (UTF8.toString fileName) SIO.WriteMode $ flip SIO.hSetFileSize fileLength

-- import qualified System.Posix.IO as PosixIO
-- import qualified "unix-bytestring" System.Posix.IO.ByteString as PosixIOBS
-- import qualified System.Posix.Files.ByteString as PosixFilesBS
writePiece :: Tracker.Tracker -> SIO.FilePath -> PieceResponse -> IO ()
writePiece tracker filePath (PieceResponse (PieceIndex pieceIndex) (PieceContent content)) = do
  fd <- PosixIO.openFd filePath PosixIO.WriteOnly Nothing PosixIO.defaultFileFlags
  written <- PosixIOBS.fdPwrite fd content $ fromIntegral $ getTrackerPieceLength tracker * pieceIndex
  PosixIO.closeFd fd
  when (fromIntegral written /= BS.length content) $
    E.throw $ userError "bad pwrite"

-- TODO: Make sure you check whether the block request is valid or not before performing it
readBlock :: Tracker.Tracker -> SIO.FilePath -> BlockRequest -> IO (BS.ByteString)
readBlock tracker filePath (BlockRequest (PieceIndex pieceIndex) (Begin begin) (RequestLength rl)) =
  SIO.withBinaryFile filePath SIO.ReadMode f
  where f h = do
          SIO.hSeek h SIO.AbsoluteSeek ((getTrackerPieceLength tracker * pieceIndex) + begin)
          BS.hGet h $ fromIntegral rl




-- TODO: Move this into an hspec
test = do
  Just t <- Tracker.testTracker2 "./test/example.torrent"

  let pieceList = getPieceList t

  let singleFileInfo@(Tracker.SingleFileInfo (Tracker.Name fileName) (Tracker.Length fileLength) _) = getTrackerSingleFileInfo t
  let pieceLength = Tracker.getTrackerPieceLength t

  print $ "singleFileInfo " ++ (show singleFileInfo)
  print $ "pieceLength " ++ (show pieceLength)
  createFile singleFileInfo
  pieceFiles <- Dir.listDirectory "pieces"

  -- Write to the file
  forM_ pieceFiles $ \file -> do
    print file
    let unhexFileName = unhex $ UTF8.fromString file
        pieces = Tracker.getTrackerPieces t
        maybePieceIndex = unhexFileName `L.elemIndex` pieces
    if isJust maybePieceIndex then do
      let pieceIndex = fromIntegral $ fromJust maybePieceIndex
      print "is an elem of pieces"
      print $ "wrinting to index " ++ (show pieceIndex) ++ " offset " ++ (show $ pieceLength * pieceIndex)
      content <- BS.readFile $ "pieces/" <> file
      writePiece t (UTF8.toString fileName) (PieceResponse (PieceIndex pieceIndex) (PieceContent content))
    else
      print "is NOT an elem of pieces"

  -- Read blocks from the file, verify that they sum up to the correct content
  forM_ pieceFiles $ \file -> do
    print file
    let unhexFileName = unhex $ UTF8.fromString file
        pieces = Tracker.getTrackerPieces t
        maybePieceIndex = unhexFileName `L.elemIndex` pieces
    if isJust maybePieceIndex then do
      print "is an elem of pieces"
      let pieceIndex = fromIntegral $ fromJust maybePieceIndex
      let (Work pi blocks) = pieceList !! (fromIntegral $ pieceIndex)
      print $ "reading index  " ++ (show pieceIndex) ++ " offset " ++ (show $ pieceLength * pieceIndex)
      content <- BS.readFile $ "pieces/" <> file
      byteStrings <- mapM (readBlock t (UTF8.toString fileName)) blocks
      print $ "content matches up: " <> (show ((BS.concat byteStrings) == content))
    else
      print "is NOT an elem of pieces"

-- test2 = do
--   Just t <- Tracker.testTracker2 "arch-spec-0.3.pdf.torrent"
--   let pieceList = getPieceList t
--   let singleFileInfo@(Tracker.SingleFileInfo (Tracker.Name bsFileName) _ _) = getTrackerSingleFileInfo t
--   let fileName = UTF8.toString bsFileName
--   fileContents <- BS.readFile fileName
--   let pieceMap = getCurrentPieceMap t fileContents
--   let filteredPieceList = maybe pieceList (\pm -> fst <$> filter (not . snd . snd) (zip pieceList pm)) pieceMap
--   print filteredPieceList
--   return filteredPieceList
