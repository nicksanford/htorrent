{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TupleSections     #-}

module FileManager where
import           Control.Concurrent                           ( ThreadId
                                                              , forkFinally
                                                              , forkIO
                                                              , threadDelay
                                                              , newMVar
                                                              , modifyMVar
                                                              , MVar (..)
                                                              )
import qualified Control.Concurrent.Chan                      as Chan
import           Control.DeepSeq                              (rnf)
import qualified Control.Exception                            as E
import           Control.Monad                                (void, when)
import qualified Data.Aeson                                   as Aeson
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Lazy                         as LBS
import qualified Data.ByteString.UTF8                         as UTF8
import qualified Data.List                                    as L
import qualified Data.Set                                     as Set
import qualified Data.List.NonEmpty                           as NonEmptyL
import qualified Data.Map                                     as M
import           Data.Maybe                                   (fromJust,
                                                               isNothing)
import           Data.Text                                    (Text)
import qualified Data.Text.Encoding                           as TE
import qualified Peer
import           Prelude                                      hiding (log)
import qualified Server
import           Shared
import qualified System.Clock                                 as Clock
import qualified System.Directory                             as Dir
import qualified System.IO                                    as SIO
import qualified System.Posix.IO                              as PosixIO
import qualified "unix-bytestring" System.Posix.IO.ByteString as PosixIOBS
import           Tracker
import           Utils                                        (shaHashRaw)
import qualified WebSocket


data FileManagerState = FileManagerState { fmOpt                      :: Opt
                                         , fmTracker                  :: Tracker
                                         , fmPieceRequest             :: Chan.Chan PieceRequest
                                         , fmResponseChan             :: Chan.Chan ResponseMessage
                                         , fmMaybeWSChan              :: Maybe (Chan.Chan Text)
                                         , fmKillChan                 :: Chan.Chan String
                                         , fmPeers                    :: [Peer]
                                         , fmPieceMap                 :: PieceMap
                                         , fmCheckouts                :: M.Map Integer (PeerThreadId, Clock.TimeSpec)
                                         , fmUnfulfilledPieceRequests :: [PieceRequest]
                                         , fmHaveIntIdxMvar           :: MVar (Set.Set Integer)
                                         }
getDefaultPieceMap :: Tracker -> [(BS.ByteString, Bool)]
getDefaultPieceMap t = (, False) <$> tPieceHashes t

getFileHashes :: Integer -> LBS.ByteString -> [BS.ByteString]
getFileHashes pieceLength = L.unfoldr f
  where f byte
          | LBS.null byte = Nothing
          | otherwise = Just (shaHashRaw $ LBS.toStrict $ LBS.take (fromIntegral pieceLength) byte,
                              LBS.drop (fromIntegral pieceLength) byte)

getCurrentPieceMap :: Tracker -> LBS.ByteString -> [(BS.ByteString, Bool)]
getCurrentPieceMap t fileContent = do
  let defaultPieceMap = fst <$> getDefaultPieceMap t
  let fileHashes = getFileHashes (fromIntegral $ tPieceLength t) fileContent
  zipWith setMatches defaultPieceMap fileHashes
  where setMatches pieceMapHash fileHash =
          if pieceMapHash == fileHash
          then (pieceMapHash, True)
          else (pieceMapHash, False)

getRequestList :: Tracker -> [BlockRequest]
getRequestList t = do
  let pieces :: [BS.ByteString ]
      pieces = tPieceHashes t
      pieceLength = tPieceLength t
      xs = [(b,min blockSize (pieceLength - b)) | b <- takeWhile (<pieceLength) $ iterate (+blockSize) 0]
      ys = [BlockRequest { bIndex = p, bBegin = b, bLength = s, bInitiator = SelfInitiated , bSentCount = 0 , bPayload = Nothing }
           | p <- [0..fromIntegral $ (length pieces) - 2], (b, s) <- xs]
      totalLength = sfLength $ tSingleFileInfo t
      remainingLength = totalLength - pieceLength * (fromIntegral $ length pieces - 1)
      lastPieceIndex = fromIntegral (length pieces) - 1
      xxs = [BlockRequest { bIndex = lastPieceIndex, bBegin = b, bLength = (min blockSize (remainingLength - b)), bInitiator = SelfInitiated , bSentCount = 0 , bPayload = Nothing }
            | b <- takeWhile (<remainingLength) $ iterate (+blockSize) 0]
  ys ++ xxs

getPieceList :: Tracker -> [PieceRequest]
getPieceList t = (\(w@(br:_)) -> PieceRequest (bIndex br) (NonEmptyL.fromList w)) <$> (L.groupBy (\brx bry -> (bIndex brx) == (bIndex bry)) $ getRequestList t)

handleResponseMsg :: FileManagerState -> ResponseMessage -> IO FileManagerState
handleResponseMsg fileManagerState response = case response of
    (Succeeded pr@PieceResponse{}) -> do
      let t = fmTracker fileManagerState
      let killChan = fmKillChan fileManagerState
      let checkouts = fmCheckouts fileManagerState
      let fileName =  UTF8.toString $ sfName $ tSingleFileInfo $ t
      let pieceMap = fmPieceMap fileManagerState
      writePiece killChan t fileName pr
      let index = presIndex pr
      let newCo = M.delete index checkouts
      let newPM = (\(pieceIndex, (k,v)) -> if pieceIndex == index then (k,True) else (k,v)) <$> zip [0..] pieceMap
      let newUnfulfilledPieceRequests = filteredWorkToBeDone (fmUnfulfilledPieceRequests fileManagerState) newPM
      maybe (return ()) (\wsChan -> Chan.writeChan wsChan $ TE.decodeUtf8 $ LBS.toStrict $ Aeson.encode $ index) $ fmMaybeWSChan fileManagerState
      updatedHaveSet <- modifyMVar (fmHaveIntIdxMvar fileManagerState) (\set -> return (Set.insert index set, Set.insert index set))
      when (debug $ fmOpt fileManagerState) $
        print $ "RESPONSE CHANNEL: have set updated to " <> show updatedHaveSet
      
      return $ fileManagerState {fmPieceMap = newPM, fmCheckouts = newCo, fmUnfulfilledPieceRequests = newUnfulfilledPieceRequests}

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
        let f k (peerThreadId, heartbeatTimeSpec) (newChenckouts, workToBeReDone) =
                   if 5 > (Clock.sec $ Clock.diffTimeSpec time heartbeatTimeSpec) then
                     (M.delete k newChenckouts, (k, peerThreadId):workToBeReDone)
                   else
                     (newChenckouts, workToBeReDone)
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
  print $ "DOWNLOADED " <> (show $ calculatePercentageDone fileManagerState) <> "%"
  -- print $ "PEERS: " <> (show $ fmPeers fileManagerState) <> "%"
  -- print $ "FMCHECKOUTS: " <> (show $ fmCheckouts fileManagerState) <> "%"
  -- print $ "FMUNFULFILLEDPIECEREQUESTS: " <> (show $ fmUnfulfilledPieceRequests fileManagerState) <> "%"
  when (allPiecesWritten (fmPieceMap fileManagerState) && quitWhenDone (fmOpt fileManagerState)) $
    Chan.writeChan (fmKillChan fileManagerState) "DONE!"

  loop fileManagerState
  where allPiecesWritten = all snd

getFileContentsAndCreateIfNeeded :: Tracker -> IO LBS.ByteString
getFileContentsAndCreateIfNeeded t = do
  let singleFileInfo = tSingleFileInfo t
  let bsFileName = sfName $ tSingleFileInfo t
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
setupFilesAndCreatePieceMap t =  do
  fileContents <- getFileContentsAndCreateIfNeeded t

  let pieceMap = getCurrentPieceMap t fileContents
  E.evaluate $ rnf pieceMap
  return pieceMap

downloadedSoFar :: Tracker -> [(BS.ByteString, Bool)] -> Integer
downloadedSoFar t pieceMap = do
  let singleFileInfo = tSingleFileInfo t

  if and $ fmap snd pieceMap then
    sfLength singleFileInfo
  else
    (fromIntegral $ length $ filter snd pieceMap) * tPieceLength t

forkPeer :: Opt -> Tracker -> Chan.Chan PieceRequest -> Chan.Chan ResponseMessage -> Chan.Chan a -> PieceMap -> MVar (Set.Set Integer) -> Peer -> IO ThreadId
forkPeer o t workChan responseChan broadcastChan pieceMap haveMVar peer =
  forkFinally (Peer.start o t peer workChan responseChan broadcastChan pieceMap haveMVar) (errorHandler o peer responseChan) 

errorHandler :: (Show a, Show b) => Opt -> Peer -> Chan.Chan ResponseMessage -> Either a b -> IO ()
errorHandler o peer chan (Left x) = do
  Chan.writeChan chan $ Error peer
  when (debug o) $
    putStrLn $ "FORK FINALLY HIT ERROR ON START PEER: " ++ show peer ++ " " ++ show x
errorHandler o peer chan (Right x) = do
  Chan.writeChan chan $ Error peer
  when (debug o) $
    putStrLn $ "FORK FINALLY SUCCEEDED: " ++ show peer ++ " " ++ show x

start :: Tracker -> Opt -> Chan.Chan String -> IO ()
start t o killChan = do
  pieceMap <- setupFilesAndCreatePieceMap t
  let l = log o
  let maybeWSP = maybeWSPort o

  workC <- Chan.newChan
  responseC <- Chan.newChan
  broadcastC <- Chan.newChan
  wsC <- Chan.newChan
  let maybeWSC = const wsC <$> maybeWSP

  haveMVar <- newMVar Set.empty
  _ <- forkIO $ Server.start o t workC responseC broadcastC pieceMap haveMVar

  maybeTrackerResponse <- trackerRequest t o (downloadedSoFar t pieceMap)
  when (isNothing maybeTrackerResponse) $
    Chan.writeChan killChan "ERROR: got empty tracker response"

  let trackerResponse = fromJust maybeTrackerResponse
  let peers = trPeers trackerResponse

  l "spawning child threads for peers"
  mapM_ (forkPeer o t workC responseC broadcastC pieceMap haveMVar) peers
  _ <- forkIO $ checkoutTimer responseC

  let workToBeDone :: [PieceRequest]
      workToBeDone = getPieceList t
  let unfulfilledPieceRequests = filteredWorkToBeDone workToBeDone pieceMap
  Chan.writeList2Chan workC unfulfilledPieceRequests
  maybe (return ()) (\wsPort -> do
                        void $ forkIO $ WebSocket.start wsPort wsC
                        Chan.writeChan wsC $ TE.decodeUtf8 $ LBS.toStrict $ Aeson.encode $ snd <$> pieceMap
                    ) maybeWSP
  loop $ FileManagerState o t workC responseC maybeWSC killChan peers pieceMap M.empty unfulfilledPieceRequests haveMVar

filteredWorkToBeDone :: [PieceRequest] -> PieceMap -> [PieceRequest]
filteredWorkToBeDone pieceRequest pieceMap = fst <$> filter (not . snd . snd) (zip pieceRequest pieceMap)

checkoutTimer :: Chan.Chan ResponseMessage -> IO ()
checkoutTimer responseC = do
  threadDelay 1000000
  Chan.writeChan responseC CheckWork

createFile :: SingleFileInfo -> IO ()
createFile sfi =
  SIO.withBinaryFile (UTF8.toString $ sfName sfi) SIO.WriteMode $ flip SIO.hSetFileSize $ sfLength sfi

writePiece :: Chan.Chan String ->  Tracker -> SIO.FilePath -> PieceResponse -> IO ()
writePiece killChan t filePath pieceResponse = do
  let index = presIndex pieceResponse
      content = piecePayload pieceResponse
  fd <- PosixIO.openFd filePath PosixIO.WriteOnly Nothing PosixIO.defaultFileFlags
  written <- PosixIOBS.fdPwrite fd content $ fromIntegral $ tPieceLength t * index
  PosixIO.closeFd fd
  when (fromIntegral written /= BS.length content) $
    Chan.writeChan killChan "ERROR: bad pwrite"
