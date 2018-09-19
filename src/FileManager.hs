{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TupleSections     #-}

module FileManager where
import           Prelude hiding (log)
import           Control.Concurrent                           (ThreadId,
                                                               forkFinally,
                                                               forkIO,
                                                               threadDelay)
import qualified Control.Concurrent.Chan                      as Chan
import qualified Control.Concurrent.STM.TChan                 as TChan
import           Control.DeepSeq                              (rnf)
import qualified Control.Exception                            as E
import           Control.Monad                                (unless, when)
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.ByteString.UTF8                         as UTF8
import           Data.Foldable                                (forM_)
import qualified Data.List                                    as L
import qualified Data.List.NonEmpty                           as NonEmptyL
import qualified Data.Map                                     as M
import           Data.Maybe                                   (fromJust, isJust,
                                                               isNothing)
import qualified Peer                                         as Peer
import qualified Server
import           Shared
import qualified System.Clock                                 as Clock
import qualified System.Directory                             as Dir
import           System.Exit                                  (exitSuccess)
import qualified System.IO                                    as SIO
import qualified System.IO                                    as SIO
import qualified System.Posix.Files.ByteString                as PosixFilesBS
import qualified System.Posix.IO                              as PosixIO
import qualified "unix-bytestring" System.Posix.IO.ByteString as PosixIOBS
import           Tracker
import           Utils                                        (shaHash,
                                                               shaHashRaw,
                                                               unhex)


getDefaultPieceMap :: Tracker -> [(BS.ByteString, Bool)]
getDefaultPieceMap tracker = (, False) <$> tPieceHashes tracker

getFileHashes :: Integer -> LBS.ByteString -> [BS.ByteString]
getFileHashes pieceLength fileContents = L.unfoldr f fileContents
  where f byte
          | LBS.null byte = Nothing
          | otherwise = Just (shaHashRaw $ LBS.toStrict $ LBS.take (fromIntegral pieceLength) byte,
                              LBS.drop (fromIntegral pieceLength) byte)

getCurrentPieceMap :: Tracker -> LBS.ByteString -> [(BS.ByteString, Bool)]
getCurrentPieceMap tracker fileContent = do
  let defaultPieceMap = fst <$> getDefaultPieceMap tracker
  let fileHashes = getFileHashes (fromIntegral $ tPieceLength tracker) fileContent
  zipWith setMatches defaultPieceMap fileHashes
  where setMatches pieceMapHash fileHash =
          if pieceMapHash == fileHash
          then (pieceMapHash, True)
          else (pieceMapHash, False)

getRequestList :: Tracker -> [BlockRequest]
getRequestList tracker = do
  let pieces :: [BS.ByteString ]
      pieces = tPieceHashes tracker
      pieceLength = tPieceLength tracker
      xs = [(b,min blockSize (pieceLength - b)) | b <- takeWhile (<pieceLength) $ iterate (+blockSize) 0]
      ys = [BlockRequest { bIndex = p, bBegin = b, bLength = s, bInitiator = SelfInitiated , bSentCount = 0 , bPayload = Nothing }
           | p <- [0..fromIntegral $ (length pieces) - 2], (b, s) <- xs]
      totalLength = sfLength $ tSingleFileInfo tracker
      remainingLength = totalLength - pieceLength * (fromIntegral $ length pieces - 1)
      lastPieceIndex = fromIntegral (length pieces) - 1
      xxs = [BlockRequest { bIndex = lastPieceIndex, bBegin = b, bLength = (min blockSize (remainingLength - b)), bInitiator = SelfInitiated , bSentCount = 0 , bPayload = Nothing }
            | b <- takeWhile (<remainingLength) $ iterate (+blockSize) 0]
  ys ++ xxs

getPieceList :: Tracker -> [PieceRequest]
getPieceList tracker = (\(w@(br:_)) -> PieceRequest (bIndex br) (NonEmptyL.fromList w)) <$> (L.groupBy (\brx bry -> (bIndex brx) == (bIndex bry)) $ getRequestList tracker)

loop opt tracker workChan responseChan peers killChan pieceMap checkouts filteredWorkToBeDone = do
  response <- Chan.readChan responseChan
  let sfi = tSingleFileInfo tracker
      bsFileName = sfName sfi
      fileLength = sfLength sfi
  let fileName = UTF8.toString bsFileName

  (newPeers, newPieceMap, newCheckouts) <- case response of
    (Succeeded (pr@PieceResponse{})) -> do
      writePiece killChan tracker fileName pr
      let index = presIndex pr
      let newCo = M.delete index checkouts
      let newPM = (\(pieceIndex, (k,v)) -> if pieceIndex == index then (k,True) else (k,v)) <$> zip [0..] pieceMap
      return (peers, newPM, newCo)
    (Error p) -> do
      let n = filter (/=p) peers
      return (n, pieceMap, checkouts)
    co@(CheckOut peerThreadId pieceIndex timestamp) -> do
      when (debug opt) $ 
        (print $ "RESPONSE CHANNEL: hit got checkout " ++ (show co)) 
      if M.member pieceIndex checkouts then do
        Chan.writeChan killChan $ "ERROR: Got checkout message for work already checkedout this should be impossible " ++ (show co)
        return (peers, pieceMap, checkouts)
      else do
        let new = M.insert pieceIndex (peerThreadId, timestamp) checkouts
        return (peers, pieceMap, new)
    hb@(HeartBeat peerThreadId pieceIndex) -> do
      let look = M.lookup pieceIndex checkouts

      when (debug opt) $ 
        print $ "Got heartbeat " <> (show $  hb)
      if isNothing look then do
        Chan.writeChan killChan $ "ERROR: Got heartbeat message for something not checked out, this should be impossible " ++ (show $  hb)
        return (peers, pieceMap, checkouts)
      else do
        time <- Clock.getTime Clock.Monotonic
        let newCo = M.adjust (\(peerThreadId, _) -> (peerThreadId, time)) pieceIndex checkouts
        return (peers, pieceMap, newCo)
    (Failed f) -> do
      when (debug opt) $ 
        print $ "RESPONSE CHANNEL: hit failure " ++ (show f)
      let index = preqIndex f
      let newCo = M.delete index checkouts
      Chan.writeChan workChan f
      return (peers, pieceMap, newCo)
    CheckWork -> do
        time <- Clock.getTime Clock.Monotonic
        let f = (\k (peerThreadId, heartbeatTimeSpec) (newChenckouts, workToBeReDone) ->
                   if 5 > (Clock.sec $ Clock.diffTimeSpec time heartbeatTimeSpec) then
                     (M.delete k newChenckouts, (k, peerThreadId):workToBeReDone)
                   else
                     (newChenckouts, workToBeReDone)
                )
        let (newCo, newWork)= M.foldrWithKey f (checkouts, []) checkouts
        let newWorkIndxs :: [Integer]
            (newWorkIndxs, _) = unzip newWork
        let regeneratedWork = filter (\pr -> (preqIndex pr) `elem` newWorkIndxs) filteredWorkToBeDone
        Chan.writeList2Chan workChan regeneratedWork
        _ <- forkIO $ checkoutTimer responseChan
        return (peers, pieceMap, newCo)
    CheckPeers ->
        return (peers, pieceMap, checkouts)

  print $ "Downloaded " ++ (show $ floor ((((fromIntegral $ length (filter (snd) newPieceMap)) / (fromIntegral $ length newPieceMap))) * 100)) ++ "%"
  when (allPiecesWritten newPieceMap && quitWhenDone opt) $
    Chan.writeChan killChan "DONE!"

  loop opt tracker workChan responseChan newPeers killChan newPieceMap newCheckouts filteredWorkToBeDone
  where allPiecesWritten = all snd


getFileContentsAndCreateIfNeeded :: Tracker -> IO LBS.ByteString
getFileContentsAndCreateIfNeeded tracker = do
  let singleFileInfo = tSingleFileInfo tracker
  let bsFileName = sfName $ tSingleFileInfo tracker
  let fileName = UTF8.toString bsFileName
  fileExists <- Dir.doesFileExist fileName

  if fileExists then do

    fileSize <- SIO.withBinaryFile fileName SIO.ReadMode SIO.hFileSize
    if fileSize /= sfLength singleFileInfo then
      createFile singleFileInfo >>
      LBS.readFile fileName
    else
      LBS.readFile fileName

  else
    createFile singleFileInfo >>
    LBS.readFile fileName

setupFilesAndCreatePieceMap :: Tracker -> IO [(BS.ByteString, Bool)]
setupFilesAndCreatePieceMap tracker =  do
  fileContents <- getFileContentsAndCreateIfNeeded tracker

  let pieceMap = getCurrentPieceMap tracker fileContents
  E.evaluate $ rnf pieceMap
  return pieceMap

downloadedSoFar :: Tracker -> [(BS.ByteString, Bool)] -> Integer
downloadedSoFar tracker pieceMap = do
  let singleFileInfo = tSingleFileInfo tracker

  if and $ fmap snd pieceMap then
    sfLength singleFileInfo
  else
    (fromIntegral $ length $ filter snd pieceMap) * tPieceLength tracker

forkPeer :: Opt -> Tracker -> Chan.Chan PieceRequest -> Chan.Chan ResponseMessage -> Chan.Chan a -> PieceMap -> Peer  -> IO ThreadId
forkPeer opt tracker workChan responseChan broadcastChan pieceMap peer =
  forkFinally (Peer.start opt tracker peer workChan responseChan broadcastChan pieceMap) (errorHandler opt peer responseChan)

errorHandler :: (Show a, Show b) => Opt -> Peer -> Chan.Chan ResponseMessage -> Either a b -> IO ()
errorHandler opt peer chan (Left x) = do
  Chan.writeChan chan $ Error peer
  when (debug opt) $ 
    putStrLn $ "FORK FINALLY HIT ERROR ON START PEER: " ++ show peer ++ " " ++ show x
errorHandler opt peer chan (Right x) = do
  Chan.writeChan chan $ Error peer
  when (debug opt) $ 
    putStrLn $ "FORK FINALLY SUCCEEDED: " ++ show peer ++ " " ++ show x

start :: Tracker -> Opt -> Chan.Chan String -> IO ()
start tracker opt killChan = do
  pieceMap <- setupFilesAndCreatePieceMap tracker
  let l = log opt

  workChan <- Chan.newChan
  responseChan <- Chan.newChan
  broadcastChan <- Chan.newChan
  _ <- forkIO $ Server.start opt tracker workChan responseChan broadcastChan pieceMap

  maybeTrackerResponse <- trackerRequest tracker opt (downloadedSoFar tracker pieceMap)
  when (isNothing maybeTrackerResponse) $ do
    Chan.writeChan killChan "ERROR: got empty tracker response"

  let trackerResponse = fromJust maybeTrackerResponse
  let peers = trPeers trackerResponse

  l "spawning child threads for peers"
  mapM_ (forkPeer opt tracker workChan responseChan broadcastChan pieceMap) peers
  _ <- forkIO $ checkoutTimer responseChan

  let workToBeDone :: [PieceRequest]
      workToBeDone = getPieceList tracker
  let filteredWorkToBeDone = fst <$> filter (not . snd . snd) (zip workToBeDone pieceMap)
  Chan.writeList2Chan workChan filteredWorkToBeDone
  loop opt tracker workChan responseChan peers killChan pieceMap M.empty filteredWorkToBeDone

checkoutTimer :: Chan.Chan ResponseMessage -> IO ()
checkoutTimer responseChan = do
  threadDelay 1000000
  Chan.writeChan responseChan CheckWork

createFile :: SingleFileInfo -> IO ()
createFile sfi =
  SIO.withBinaryFile (UTF8.toString $ sfName sfi) SIO.WriteMode $ flip SIO.hSetFileSize $ sfLength sfi

writePiece :: Chan.Chan String ->  Tracker -> SIO.FilePath -> PieceResponse -> IO ()
writePiece killChan tracker filePath pieceResponse = do
  let index = presIndex pieceResponse
      content = piecePayload pieceResponse
  fd <- PosixIO.openFd filePath PosixIO.WriteOnly Nothing PosixIO.defaultFileFlags
  written <- PosixIOBS.fdPwrite fd content $ fromIntegral $ tPieceLength tracker * index
  PosixIO.closeFd fd
  when (fromIntegral written /= BS.length content) $
    Chan.writeChan killChan "ERROR: bad pwrite"
