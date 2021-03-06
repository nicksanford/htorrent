{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TupleSections     #-}

module FSM ( recvLoop
           , pieceMapToBitField
           , fetchBlockResponses
           , buildFSMState
           , blockResponseToBS
           ) where

import           Control.Concurrent.Chan                      as Chan
import           Control.DeepSeq                              (rnf)
import qualified Control.Exception                            as E
import           Control.Monad                                (when)
import qualified Data.Bits.Bitwise                            as Bitwise
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.UTF8                         as UTF8
import           Data.Foldable                                (find, foldl')
import           Data.List                                    (sortOn)
import qualified Data.List.NonEmpty                           as NonEmptyL
import           Data.Maybe                                   (fromMaybe,
                                                               isJust,
                                                               isNothing,
                                                               listToMaybe)
import           Network.Socket                               hiding (recv)
import           Network.Socket.ByteString                    (recv, sendAll)
import           Parser                                       (rpcParse)
import           RPCMessages                                  (keepAlive,
                                                               request)
import           Shared
import qualified System.Clock                                 as Clock
import qualified System.Posix.IO                              as PosixIO
import qualified "unix-bytestring" System.Posix.IO.ByteString as PosixIOBS
import           System.Timeout                               (timeout)
import           Utils

requestLimit :: Int
requestLimit = 10

buildFSMState :: Opt -> Tracker -> BS.ByteString -> BS.ByteString -> Socket -> Chan.Chan PieceRequest -> Chan.Chan ResponseMessage -> Clock.TimeSpec -> PieceMap -> Initiator -> FSMState
buildFSMState o t fsmStateId peerID conn workC responseC time pieceMap fsmInitiator =
  let selfState = SelfState { selfId                   = (tPeerId t)
                            , selfPieceMap             = pieceMap
                            , selfChokingPeer          = False
                            , selfInterestedInPeer     = True
                            }

      peerState = PeerState { peerId                   = peerID
                            , peerPieceMap             = initPieceMap t
                            , peerPieceRequestFromSelf = Nothing
                            , blockResponsesForPeer    = []
                            , peerChokingMe            = True
                            , peerInterestedInMe       = False
                            , lastHeartBeat            = Nothing
                            , lastKeepAlive            = time
                            }
  in FSMState fsmStateId t conn peerState selfState workC responseC "" [] fsmInitiator o

-- TODO Got to figure out how to send a keep alive to every peer every 30 seconds w/o blocking the thread
recvLoop :: FSMState -> IO ()
--recvLoop tracker peerState@(PeerState (PeerId peer_id) (Conn conn) pieceMap) peerRPCParse workChan responseChan currentWork = do
recvLoop fsmState = do -- @(PeerState (PeerId peer_id) (Conn conn) _ _ (RPCParse peerRPCParse) _ _ _ _ (TimeStamps (_, (LastKeepAlive lastKeepAlive))) pieces) = do
  -- TODO If anything throws, catch it, put the work back in the response queue for the parent thread
  let peerState = getPeer fsmState
  let conn = getConn fsmState
  fsmLog fsmState $ "Blocked on recvLoop, peerState: " <> (show fsmState)
  fsmLog fsmState $ "RPCMSG: " <> ((show . parsedRPCs) fsmState)
  -- TODO If this is null but you still have checked out work, put the work back, I'm pretty sure I only need to do this on recv calls

  now <- Clock.getTime Clock.Monotonic
  let newlastKeepAlive = (newKeepAlive now $ lastKeepAlive peerState)
  when (newlastKeepAlive == now) $ sendAll conn keepAlive

  msg <- recv conn 16384
--  BS.appendFile (UTF8.toString $ "./vcr/" <> fsmId fsmState) msg

--  fsmLog  fsmState $ " msg: " <> (show $ BS.unpack msg)
  --let newPeerRPCParse = parseRPC (selfPieceMap selfState) msg $ rpcParse fsmState
  let eitherNewRPCParse = rpcParse (unparsedRPCs fsmState) msg
  E.evaluate $ rnf eitherNewRPCParse

  case eitherNewRPCParse of
    (Left e) -> do
      fsmLog fsmState $ "ERROR: RECVLOOP hit parse error " <> e
      _ <- sendWorkBackToManager fsmState
      return ()
    Right newRPCParse ->
      if BS.null msg then do
        fsmLog fsmState $ "DONE: RECVLOOP got null in receive"
        _ <- sendWorkBackToManager fsmState
        return ()
      else do
        let eitherUpdatedFSMState = updateFsmState fsmState newRPCParse newlastKeepAlive
        case eitherUpdatedFSMState of
          Left e -> do
            fsmLog fsmState $ "ERROR: RECVLOOP updateFsmState " <> e
            _ <- sendWorkBackToManager fsmState
            return ()
          Right updatedFSMState ->
            if peerChokingMe $ getPeer updatedFSMState then do
              -- TODO: clear your work state and send it back to the parent
              fsmLog  fsmState "Peer choking, can't ask it for pieces"
              updatedFSMStateWithoutWork <- sendWorkBackToManager updatedFSMState
                                              >>= buildPieces
                                              >>= sendPieces
              recvLoop updatedFSMStateWithoutWork
            else do
              fsmLog  fsmState "Peer NOT choking, can potentially ask for pieces"
              finalPeerState <- sendFinishedWorkBackToManager updatedFSMState
                                    >>= buildPieces
                                    >>= sendPieces
                                    >>= tryToPullWork
                                    >>= sendRequests


              recvLoop finalPeerState

maybeGetNewWork :: FSMState -> IO (Maybe PieceRequest)
maybeGetNewWork fsmState =
  if isNothing (peerPieceRequestFromSelf $ getPeer $ fsmState) then do
    fsmLog  fsmState "WORK IS EMPTY PULLING NEW WORK"
    loop 0
  else
    return Nothing
  where sPieceMap = selfPieceMap $ getSelf fsmState
        maxNumberofPiecesLeft = length $ filter (not . snd) sPieceMap
-- TODO - Change this to use getChannelContents
        loop :: Int -> IO (Maybe PieceRequest)
        loop count = do
          fsmLog  fsmState "going into loop"
          if (count >= maxNumberofPiecesLeft) then do
            fsmLog  fsmState $ "looped through " <> (show count) <> " work chan messages before quitting"
            return Nothing
          else do
            maybeWork <- timeout 100 (Chan.readChan $ workChan fsmState)
            case maybeWork of
              Nothing -> do
                fsmLog  fsmState $ "TRIED PULLING WORK BUT IT TIMED OUT"
                return Nothing
              Just pieceRequest -> do

                fsmLog  fsmState $ "pieces that can be done " <> (show $ piecesThatCanBeDone $ peerPieceMap $ getPeer fsmState)
                if peerHasData fsmState pieceRequest then do
                  currentTime <- Clock.getTime Clock.Monotonic
                  Chan.writeChan (responseChan fsmState) (CheckOut (fsmId fsmState) (preqIndex pieceRequest) currentTime)
                  return $ Just pieceRequest
                else do
                  fsmLog  fsmState $ "WORK CAN'T BE DONE, ON INDEX " <> (show pi) <> "BUT CAN BE DONE ON " <> (show $ piecesThatCanBeDone $ peerPieceMap $ getPeer fsmState) <> " SENDING BACK TO MANAGER AND TRYING AGAIN"
                  Chan.writeChan (responseChan fsmState) (Failed $ PieceRequest (preqIndex pieceRequest) $ preqBlockRequests pieceRequest)
                  loop (count + 1)

findBitField :: PeerRPC -> Bool
findBitField (BitField _) = True
findBitField _            = False

updateFsmState :: FSMState -> (BS.ByteString, [PeerRPC]) -> Clock.TimeSpec -> Either String FSMState
updateFsmState fsmState (unparsed, parsed) _newKeepAliveTime = do
  let peer = getPeer fsmState
  let oldPieceMap = peerPieceMap peer
  let bitfieldUpdatedPieceMap = maybe oldPieceMap
                                (mergeBitFieldIntoPieceMap oldPieceMap)
                                $ (\(BitField piecemap) -> piecemap)
                                <$> (find findBitField (reverse parsed))
  let newPeerPieceMap = foldr updatePieceWithHave bitfieldUpdatedPieceMap parsed
  let newPeerChoking = foldl' updatePeerChoking (peerChokingMe peer) parsed
  let newPieceRequest = mergeResponsesIntoWork (fmap (\(Response br) -> br) $ filter onlyResponses parsed) <$> (peerPieceRequestFromSelf peer)
  let newPeerInterested = (Interested ==) <$> (listToMaybe . reverse $ filter onlyInterestedOrNotInterested parsed)
  let newPeer = peer { peerPieceMap = newPeerPieceMap
                     , peerChokingMe = newPeerChoking
                     , peerInterestedInMe = fromMaybe (peerInterestedInMe peer) newPeerInterested
                     , peerPieceRequestFromSelf = newPieceRequest
                     }
  let newPeerRPCs = filter clearConsumedRPCs parsed
  --FSMState id singleFileInfo pieceLength conn newPeer self wc rc (PeerRPCParse w8 e newPeerRPCs) newMaybeWork p lhb newKeepAliveTime initiator
  Right $ fsmState {getPeer = newPeer, unparsedRPCs = unparsed, parsedRPCs = newPeerRPCs}
  where
        clearConsumedRPCs (Have _)      = False
        clearConsumedRPCs (BitField _)  = False
        clearConsumedRPCs Choke         = False
        clearConsumedRPCs UnChoke       = False
        clearConsumedRPCs Interested    = False
        clearConsumedRPCs NotInterested = False
        clearConsumedRPCs Response{}    = False
        clearConsumedRPCs _             = True
        updatePieceWithHave (Have key) acc = fmap (\(i, (k,v)) -> if i == key then (k,True) else (k,v)) (zip [0..] acc)
        updatePieceWithHave _ acc = acc
        updatePeerChoking :: Bool -> PeerRPC -> Bool
        updatePeerChoking _ Choke   = True
        updatePeerChoking _ UnChoke = False
        updatePeerChoking acc _     = acc
        onlyResponses Response{} = True
        onlyResponses _          = False
        onlyInterestedOrNotInterested Interested    = True
        onlyInterestedOrNotInterested NotInterested = True
        onlyInterestedOrNotInterested _             = False
        mergeResponsesIntoWork :: [BlockResponse] -> PieceRequest -> PieceRequest
        mergeResponsesIntoWork responses pieceRequest = do
          let updatedBlockRequests = NonEmptyL.fromList $ (maybeUpdateBlockRequest responses)
                                                       <$> (NonEmptyL.toList $ preqBlockRequests pieceRequest)
          pieceRequest {preqBlockRequests = updatedBlockRequests}

maybeUpdateBlockRequest :: [BlockResponse] -> BlockRequest -> BlockRequest
maybeUpdateBlockRequest responses blockRequest =
  maybe blockRequest (\x -> blockRequest { bPayload = Just (pBlock x) }) (find f responses)
  where f blockResponse =
          isNothing (bPayload blockRequest) &&
          pIndex blockResponse == bIndex blockRequest &&
          pBegin blockResponse == (bBegin blockRequest)

-- clearWork :: FSMState -> FSMState
-- clearWork (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc _ p lhb lka initiator) =
--   (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc [] p lhb lka initiator)

-- clearPieces :: FSMState -> FSMState
-- clearPieces (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc work pieces lhb lka initiator) =
--   (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc work [] lhb lka initiator)

sendWorkBackToManager :: FSMState -> IO FSMState
sendWorkBackToManager fsmState =
  maybe (return fsmState) f (peerPieceRequestFromSelf peer)
  where peer = getPeer fsmState
        f pieceRequest = do
          Chan.writeChan (responseChan fsmState) (Failed pieceRequest)
          return $ fsmState { getPeer = peer { peerPieceRequestFromSelf = Nothing }}

sendFinishedWorkBackToManager :: FSMState -> IO FSMState
sendFinishedWorkBackToManager fsmState = do
  let peerState = getPeer fsmState
  let selfPM = selfPieceMap $ getSelf fsmState
  let peerPieceRequest = peerPieceRequestFromSelf peerState
  let maybePieceRequests = hasAllPayloads =<< peerPieceRequest
  case maybePieceRequests of
    Nothing ->
      return fsmState
    Just (validPeerPieceRequest, payloads) -> do
      case conformsToHash selfPM validPeerPieceRequest payloads of
        Just content -> do
          let success = Succeeded (PieceResponse (preqIndex validPeerPieceRequest) content)
          Chan.writeChan (responseChan fsmState) success
          let newPeer = peerState { peerPieceRequestFromSelf = Nothing }
          return $ fsmState { getPeer = newPeer}
        Nothing -> do
          -- DONE log and shed the broken state
          fsmLog  fsmState $ "ERROR: piece " <> (show $ preqIndex validPeerPieceRequest) <> " does does not conform to hash"
          sendWorkBackToManager fsmState

hasAllPayloads :: PieceRequest -> Maybe (PieceRequest, NonEmptyL.NonEmpty BS.ByteString)
hasAllPayloads pieceRequests = do
  list <- traverse bPayload (preqBlockRequests pieceRequests)
  return (pieceRequests, list)

conformsToHash :: PieceMap -> PieceRequest -> NonEmptyL.NonEmpty BS.ByteString -> Maybe BS.ByteString
conformsToHash pieceMap pieceRequest payloads = do
--  let sortedBlockRequests = NonEmptyL.sortBy (\a b -> bBegin a `compare` bBegin b) (preqBlockRequests pieceRequests)
--  let maybeNonEmptyPayloadList = traverse (\br -> (\p -> (bBegin br, p)) <$> bPayload br ) sortedBlockRequests
  let combinedPayload = BS.concat $ (NonEmptyL.toList payloads)
  let (expectedSha, _) = pieceMap !! (fromIntegral $ preqIndex pieceRequest)
  if expectedSha == shaHashRaw combinedPayload then
    Just combinedPayload
  else
    Nothing

initPieceMap :: Tracker -> PieceMap
initPieceMap t = (, False) <$> tPieceHashes t

fetchBlockResponses :: Integer -> SingleFileInfo -> [PeerRPC] -> IO [BlockResponse]
fetchBlockResponses pieceLen singleFileInfo rpcs = do
  let fileName = sfName singleFileInfo
  mapM (r $ UTF8.toString fileName) $ filter isRequest rpcs
  where r fileName (Request blockRequest) = do
          let index = bIndex blockRequest
          let begin = bBegin blockRequest
          let len =  bLength blockRequest
          fd <- PosixIO.openFd fileName PosixIO.ReadOnly Nothing PosixIO.defaultFileFlags
          readBS <- PosixIOBS.fdPread fd (fromIntegral $ len) (fromIntegral $ (pieceLen * index) + begin)
          PosixIO.closeFd fd
          return $ BlockResponse index begin readBS

isRequest :: PeerRPC -> Bool
isRequest Request{} = True
isRequest _         = False

buildPieces :: FSMState -> IO FSMState
buildPieces fsmState = do -- @(FSMState fsmID singleFileInfo pieceLength conn peer self wc rc rpc work pieces lhb lka initiator) = do-- (PeerState a b c d (RPCParse (PeerRPCParse buffer err parsedRPCs)) f g h i j pieces) = do
  let peer = getPeer fsmState
  let oldParsedRPCs = parsedRPCs fsmState
  let fsmTracker = getTracker fsmState
  newBlockResponses <- fetchBlockResponses (tPieceLength fsmTracker) (tSingleFileInfo fsmTracker) oldParsedRPCs
  let newRPC = filter (not . isRequest) oldParsedRPCs
  return $ fsmState { parsedRPCs = newRPC
                    , getPeer = peer { blockResponsesForPeer = newBlockResponses }
                    } -- $ FSMState fsmID singleFileInfo pieceLength conn peer self wc rc newRPC work newPieces lhb lka initiator

sendPieces :: FSMState -> IO FSMState
sendPieces fsmState = do --(FSMState a (Conn conn) c d (RPCParse (PeerRPCParse buffer err parsedRPCs)) f g h i j pieces) = do
  let peer = getPeer fsmState
      bs = BS.concat (blockResponseToBS <$> blockResponsesForPeer peer)
  fsmLog  fsmState $ "SENDING PIECES: " <> (show $ blockResponsesForPeer peer)
  sendAll (getConn fsmState) bs
  return $ fsmState { getPeer = peer { blockResponsesForPeer = []}}

blockResponseToBS :: BlockResponse -> BS.ByteString
blockResponseToBS (BlockResponse index begin content) =
  (BS.pack ((BS.unpack $ integerToBigEndian $ (fromIntegral $ BS.length content) + 9) <>
  [7] <>
  (BS.unpack $ integerToBigEndian $ fromIntegral index) <> (BS.unpack $ integerToBigEndian $ fromIntegral begin)))
  <> content

newKeepAlive :: Clock.TimeSpec -> Clock.TimeSpec -> Clock.TimeSpec
newKeepAlive now old =
  if 60 < (Clock.sec $ Clock.diffTimeSpec now old)
  then now
  else old

filterCompletedBlocks :: BlockRequest -> Bool
filterCompletedBlocks = isJust . bPayload

filterWorkingBlocks :: BlockRequest -> Bool
filterWorkingBlocks b = (bSentCount b /= 0) && (isNothing $ bPayload b)-- (Block _ _ _ (SentTimestamp (Just _)) _ Nothing) = True

filterUnworkedBlocks :: BlockRequest -> Bool
filterUnworkedBlocks b = (bSentCount b == 0) && (isNothing $ bPayload b)-- (Block _ _ _ (SentTimestamp Nothing) _ Nothing) = True

sendRequests :: FSMState -> IO FSMState
sendRequests fsmState = do
  let peer = getPeer fsmState
  case peerPieceRequestFromSelf peer of
    Nothing ->
      return fsmState
    Just pieceRequest -> do
      let blockRequests = NonEmptyL.toList $ preqBlockRequests pieceRequest
      let completedRequests = filter filterCompletedBlocks blockRequests
      let workingRequests = filter filterWorkingBlocks blockRequests
      let unworkedRequests = filter filterUnworkedBlocks blockRequests
      let newRequestsToWork = take (requestLimit - (length workingRequests)) unworkedRequests
      let unworkedRequestsMinusNewRequestsToWork = drop (requestLimit - (length workingRequests)) unworkedRequests
      newWork <- traverse f newRequestsToWork
      let nextBlockRequests = sortOn bBegin (newWork <>
                                             unworkedRequestsMinusNewRequestsToWork <>
                                             workingRequests <>
                                             completedRequests)
      let newPieceRequest = pieceRequest { preqBlockRequests = NonEmptyL.fromList nextBlockRequests }
      return $ fsmState {getPeer = peer {peerPieceRequestFromSelf = Just newPieceRequest}}
  where
        f :: BlockRequest -> IO BlockRequest
        f blockRequest = do
            let r = request (bIndex blockRequest) (bBegin blockRequest) (bLength blockRequest)
            fsmLog  fsmState $ " SENDING REQUEST " <> (show blockRequest)
            sendAll (getConn fsmState) r
            let newSentCount = (bSentCount blockRequest) + 1
            return $ blockRequest {bSentCount = newSentCount}

pieceMapToBitField :: PieceMap -> BS.ByteString
pieceMapToBitField pieceMap = do
  let chunks :: [[Bool]]
      chunks = (chunkWithDefault False 8 $ fmap snd pieceMap)
  let x = fmap (\(a:b:c:d:e:f:g:h:[]) -> Bitwise.packWord8BE a b c d e f g h) chunks
  let bitfield :: BS.ByteString
      bitfield = BS.pack x
      len = BS.unpack $ integerToBigEndian $ fromIntegral $ BS.length bitfield + 1
  BS.pack (len <> [5]) <> bitfield

mergeBitFieldIntoPieceMap :: PieceMap -> [Bool] -> PieceMap
mergeBitFieldIntoPieceMap = zipWith (\(x,xbool) ybool -> (x, xbool || ybool))

peerHasData :: FSMState -> PieceRequest -> Bool
peerHasData fSMState pieceRequest =
  (preqIndex pieceRequest) `elem` (piecesThatCanBeDone $ peerPieceMap $ getPeer fSMState)

piecesThatCanBeDone :: PieceMap -> [Integer]
piecesThatCanBeDone pieceMap = fmap fst $ filter (snd . snd) $ zip [0..] pieceMap

-- https://wiki.haskell.org/Data.List.Split
chunkWithDefault :: a -> Int -> [a] -> [[a]]
chunkWithDefault _ _ [] = []
chunkWithDefault d i xs =
  addDefault y : chunkWithDefault d i ys
  where (y, ys) = splitAt i xs
        addDefault z
          | length z == i = z
          | otherwise = z <> replicate (i - length z) d

tryToPullWork :: FSMState-> IO FSMState
tryToPullWork fsmState = do
  maybeNewWork <- maybeGetNewWork fsmState
  let peer = getPeer fsmState
  return $ maybe fsmState (\newPieceRequest -> fsmState {getPeer = peer { peerPieceRequestFromSelf = Just newPieceRequest }}) maybeNewWork
