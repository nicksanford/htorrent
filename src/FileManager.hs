{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TupleSections     #-}

module FileManager where
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import           Prelude hiding (log)
import           Control.Concurrent                           ( ThreadId
                                                              , forkFinally
                                                              , forkIO
                                                              , threadDelay
                                                              )
import qualified Control.Concurrent.Chan                      as Chan
import           Control.DeepSeq                              (rnf)
import qualified Control.Exception                            as E
import           Control.Monad                                ( when
                                                              , void
                                                              )
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.ByteString.UTF8                         as UTF8
import Data.Text (Text, pack, unpack)
import qualified Data.List                                    as L
import qualified Data.List.NonEmpty                           as NonEmptyL
import qualified Data.Map                                     as M
import           Data.Maybe                                   ( fromJust
                                                              , isNothing
                                                              )
import qualified Peer
import qualified Server
import           Shared
import qualified System.Clock                                 as Clock
import qualified System.Directory                             as Dir
import qualified System.IO                                    as SIO
import qualified System.Posix.IO                              as PosixIO
import qualified "unix-bytestring" System.Posix.IO.ByteString as PosixIOBS
import           Tracker
import qualified WebSocket
import           Utils                                        (shaHashRaw)


data FileManagerState = FileManagerState { fmOpt :: Opt
                                         , fmTracker :: Tracker
                                         , fmPieceRequest :: Chan.Chan PieceRequest
                                         , fmResponseChan :: Chan.Chan ResponseMessage
                                         , fmMaybeWSChan :: Maybe (Chan.Chan Text)
                                         , fmKillChan :: Chan.Chan String
                                         , fmPeers :: [Peer]
                                         , fmPieceMap :: PieceMap
                                         , fmCheckouts :: M.Map Integer (PeerThreadId, Clock.TimeSpec)
                                         , fmUnfulfilledPieceRequests :: [PieceRequest]
                                         }

getDefaultPieceMap :: Tracker -> [(BS.ByteString, Bool)]
getDefaultPieceMap tracker = (, False) <$> tPieceHashes tracker

getFileHashes :: Integer -> LBS.ByteString -> [BS.ByteString]
getFileHashes pieceLength = L.unfoldr f
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

handleResponseMsg :: FileManagerState -> ResponseMessage -> IO FileManagerState
handleResponseMsg fileManagerState response = case response of
    (Succeeded pr@PieceResponse{}) -> do
      let tracker = fmTracker fileManagerState
      let killChan = fmKillChan fileManagerState
      let checkouts = fmCheckouts fileManagerState
      let fileName =  UTF8.toString $ sfName $ tSingleFileInfo $ tracker
      let pieceMap = fmPieceMap fileManagerState
      writePiece killChan tracker fileName pr
      let index = presIndex pr
      let newCo = M.delete index checkouts
      let newPM = (\(pieceIndex, (k,v)) -> if pieceIndex == index then (k,True) else (k,v)) <$> zip [0..] pieceMap
      return $ fileManagerState {fmPieceMap = newPM, fmCheckouts = newCo}

    (Error p) -> do
      let newFMPeers = filter (/=p) $ fmPeers fileManagerState
      return $ fileManagerState {fmPeers = newFMPeers}

    co@(CheckOut peerThreadId pieceIndex timestamp) -> do
      when (debug $ fmOpt fileManagerState) $
        print $ "RESPONSE CHANNEL: hit got checkout " <> show co
      if M.member pieceIndex (fmCheckouts fileManagerState) then do
        Chan.writeChan (fmKillChan fileManagerState) $ "ERROR: Got checkout message for work already checkedout this should be impossible " <> show co
        return fileManagerState
      else
        let newCheckouts = M.insert pieceIndex (peerThreadId, timestamp) $ fmCheckouts fileManagerState
        in return $ fileManagerState {fmCheckouts = newCheckouts}

    hb@(HeartBeat _peerThreadId pieceIndex) -> do
      let look = M.lookup pieceIndex $ fmCheckouts fileManagerState

      when (debug $ fmOpt fileManagerState) $
        print $ "Got heartbeat " <> show hb
      if isNothing look then do
        Chan.writeChan (fmKillChan fileManagerState) $ "ERROR: Got heartbeat message for something not checked out, this should be impossible " <> show hb
        return fileManagerState
      else do
        time <- Clock.getTime Clock.Monotonic
        let newCheckouts = M.adjust (\(peerThreadId, _) -> (peerThreadId, time)) pieceIndex $ fmCheckouts fileManagerState
        return $ fileManagerState {fmCheckouts = newCheckouts}

    (Failed f) -> do
      when (debug $ fmOpt fileManagerState) $
        print $ "RESPONSE CHANNEL: hit failure " <> show f
      let newCheckouts = M.delete (preqIndex f) $ fmCheckouts fileManagerState
      Chan.writeChan (fmPieceRequest fileManagerState) f
      return $ fileManagerState {fmCheckouts = newCheckouts}

    CheckWork -> do
        let checkouts = fmCheckouts fileManagerState
        time <- Clock.getTime Clock.Monotonic
        let f = (\k (peerThreadId, heartbeatTimeSpec) (newChenckouts, workToBeReDone) ->
                   if 5 > (Clock.sec $ Clock.diffTimeSpec time heartbeatTimeSpec) then
                     (M.delete k newChenckouts, (k, peerThreadId):workToBeReDone)
                   else
                     (newChenckouts, workToBeReDone)
                )
        let (newCheckouts, newWork)= M.foldrWithKey f (checkouts, []) checkouts
        let newWorkIndxs :: [Integer]
            (newWorkIndxs, _) = unzip newWork
        let regeneratedPieceRequests = filter (\pr -> (preqIndex pr) `elem` newWorkIndxs) $ fmUnfulfilledPieceRequests fileManagerState
        Chan.writeList2Chan (fmPieceRequest fileManagerState) regeneratedPieceRequests
        _ <- forkIO $ checkoutTimer $ fmResponseChan fileManagerState
        return $ fileManagerState {fmCheckouts = newCheckouts}

    CheckPeers ->
        -- TODO: Implement check peers
        return fileManagerState

calculatePercentageDone :: FileManagerState -> Integer
calculatePercentageDone fileManagerState =
  let pieceMap = fmPieceMap fileManagerState
      numberOfDownloadedPieces = fromIntegral $ length (filter snd pieceMap)
      totalNumberOfPieces = (fromIntegral $ length pieceMap)
  in floor $ (numberOfDownloadedPieces / totalNumberOfPieces) * 100

loop :: FileManagerState -> IO ()
loop oldFileManagerState = do
  response <- Chan.readChan $ fmResponseChan oldFileManagerState

  fileManagerState <- handleResponseMsg oldFileManagerState response
  print $ "Downloaded " <> (show $ calculatePercentageDone fileManagerState) <> "%"
  when (allPiecesWritten (fmPieceMap fileManagerState) && quitWhenDone (fmOpt fileManagerState)) $
    Chan.writeChan (fmKillChan fileManagerState) "DONE!"

  loop fileManagerState
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

setupFilesAndCreatePieceMap :: Tracker -> IO PieceMap
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

forkPeer :: Opt -> Tracker -> Chan.Chan PieceRequest -> Chan.Chan ResponseMessage -> Chan.Chan a -> PieceMap -> Peer -> IO ThreadId
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
  let maybeWSP = maybeWSPort opt

  workC <- Chan.newChan
  responseC <- Chan.newChan
  broadcastC <- Chan.newChan
  wsC <- Chan.newChan
  let maybeWSC = const wsC <$> maybeWSP

  _ <- forkIO $ Server.start opt tracker workC responseC broadcastC pieceMap

  maybeTrackerResponse <- trackerRequest tracker opt (downloadedSoFar tracker pieceMap)
  when (isNothing maybeTrackerResponse) $
    Chan.writeChan killChan "ERROR: got empty tracker response"

  let trackerResponse = fromJust maybeTrackerResponse
  let peers = trPeers trackerResponse

  l "spawning child threads for peers"
  mapM_ (forkPeer opt tracker workC responseC broadcastC pieceMap) peers
  _ <- forkIO $ checkoutTimer responseC

  let workToBeDone :: [PieceRequest]
      workToBeDone = getPieceList tracker
  let filteredWorkToBeDone = fst <$> filter (not . snd . snd) (zip workToBeDone pieceMap)
  Chan.writeList2Chan workC filteredWorkToBeDone
  maybe (return ()) (\wsPort -> do
                        Chan.writeChan wsC $ TE.decodeUtf8 $ LBS.toStrict $ Aeson.encode pieceMap
                        void $ forkIO $ WebSocket.start wsPort wsC
                    ) maybeWSP
  loop $ FileManagerState opt tracker workC responseC maybeWSC killChan peers pieceMap M.empty filteredWorkToBeDone

checkoutTimer :: Chan.Chan ResponseMessage -> IO ()
checkoutTimer responseC = do
  threadDelay 1000000
  Chan.writeChan responseC CheckWork

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
