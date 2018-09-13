{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Peer where

import qualified Tracker                   as T (InfoHash (..),
                                                 PeerId (..), Tracker (..),
                                                 SingleFileInfo (..), Name (..),
                                                 Length (..),
                                                 getTrackerPieces,
                                                 getTrackerSingleFileInfo, getTrackerPieceLength, getTrackerPeerId)
import qualified Shared               as Shared
import           Utils                     (unhex, shaHashRaw)

import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)
import qualified Control.Concurrent.Chan as Chan


import qualified Data.Bits                 as Bits
import qualified Data.Bits.Bitwise         as Bitwise
import qualified Data.Word8                as W

import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as UTF8
import qualified Data.ByteString.Base16    as B16
import qualified Data.ByteString.Lazy    as Lazy
import qualified Data.Either               as Either

import           Control.Monad             (unless, forM_, when)
import           Data.List                 (find, foldl', sortOn)
import qualified Data.Map                  as M

import           Data.Maybe                (fromJust, isNothing, listToMaybe, fromMaybe, isJust)
import qualified Data.Set as S
import qualified System.Clock as Clock
import qualified Data.Binary               as Binary
import qualified Data.Sequence             as Seq
import Data.Foldable             (toList)
import System.Timeout             (timeout)
import qualified Control.Exception as E
import qualified System.Posix.IO as PosixIO
import qualified "unix-bytestring" System.Posix.IO.ByteString as PosixIOBS
import qualified System.Posix.Files.ByteString as PosixFilesBS
import Data.Attoparsec.ByteString

newtype Conn e = Conn e deriving (Eq, Show)
newtype InfoHash e = InfoHash e deriving (Eq, Show)
newtype PeerId e = PeerId e deriving (Eq, Show)

showPeerId :: BS.ByteString -> String
showPeerId = UTF8.toString . B16.encode 

data PeerResponse = PeerResponse (InfoHash BS.ByteString) (PeerId BS.ByteString) deriving (Eq, Show)

-- TODO: I found from trying to download arch that I was getting dupes of piece shas (not sure if I am doing something wrong, but this is to ensure I don't drop pieces b/c the shas are the same)
newtype PieceMap = PieceMap [(BS.ByteString, Bool)] deriving Eq
newtype Chans a = Chans a deriving (Eq)
newtype RPCParse a = RPCParse a deriving (Eq, Show)
newtype OutStandingWork a = OutStandingWork a deriving (Eq, Show)
newtype PeerChoked a = PeerChoked a deriving (Eq, Show)
newtype PeerChoking a = PeerChoking a deriving (Eq, Show)
newtype Index a = Index a deriving (Eq, Show)
newtype Begin a = Begin a deriving (Eq, Show)
data Payload = Payload BS.ByteString deriving (Eq)
newtype SentTimestamp a = SentTimestamp a deriving (Eq, Show)
newtype Length a = Length a deriving (Eq, Show)
newtype SentCount a = SentCount a deriving (Eq, Show)
newtype LastHeartbeat a = LastHeartbeat a deriving (Eq, Show)
newtype TimeStamps a = TimeStamps a deriving (Eq, Show)
newtype LastKeepAlive a = LastKeepAlive a deriving (Eq, Show)
newtype InteresetedInPeer a = InteresetedInPeer a deriving (Eq, Show)
newtype PeerInterested a = PeerInterested a deriving (Eq, Show)
newtype PeerPieceMap a = PeerPieceMap a deriving (Eq, Show)



instance Show PieceMap where
  show (PieceMap m) = "PieceMap len: " <> (show $ length m) <> "number of set bits: " <> (show $ length $ filter snd m)

instance Show (Chans m) where
  show (Chans m) = "Chans" 

instance Show (Payload) where
  show (Payload m) = "Payload length: " <> (show $ BS.length m)

instance Show (FSMState) where
  show a = "FSMState { fsmId: "         <> (show $ fsmId a) <> ", "
                   <> "getConn: "       <> (show $ getConn a) <> ", "
                   <> "getPeer: "       <> (show $ getPeer a) <> ", "
                   <> "getSelf: "       <> (show $ getSelf a) <> ", "
                   <> "rpcParse: "      <> (show $ rpcParse a) <> ", "
                   <> "maybeWork: "     <> (show $ work a) <> ", "
                   <> "lastHeartBeat: " <> (show $ lastHeartBeat a) <> ", "
                   <> "lastKeepAlive: " <> (show $ lastKeepAlive a) <> ", "
                   <> "pieces: "        <> (show $ pieces a) <> ", "
                   <> "}" 

data FSMState = FSMState { fsmId         :: BS.ByteString
                         , getSingleFileInfo :: (BS.ByteString, Integer)
                         , getPieceLength :: Integer
                         , getConn       :: Socket
                         , getPeer       :: PeerState
                         , getSelf       :: SelfState
                         , workChan      :: Chan.Chan Shared.WorkMessage
                         , responseChan  :: Chan.Chan Shared.ResponseMessage
                         , rpcParse      :: PeerRPCParse
                         , work          :: [Block]
                         , pieces        :: [Piece]
                         , lastHeartBeat :: Maybe Clock.TimeSpec
                         , lastKeepAlive :: Clock.TimeSpec
                         , initiator     :: Initiator
                         }
                   deriving (Eq)

data PeerState = PeerState { peerId :: BS.ByteString
                           , peerPieceMap :: PieceMap
                           , peerChokingMe :: Bool
                           , peerInterestedInMe :: Bool
                           }
              deriving (Eq, Show)

data SelfState = SelfState { selfId :: BS.ByteString
                           , selfPieceMap :: PieceMap
                           , selfChokingPeer :: Bool
                           , selfInterestedInPeer :: Bool
                           }
              deriving (Eq, Show)

data Initiator = Self | Peer deriving (Eq, Show)

data PeerRPC = PeerKeepAlive
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have BS.ByteString
             | BitField PieceMap
             | Cancel Integer Integer Integer
             | Request Integer Integer Integer
             | Response Integer Integer BS.ByteString
             deriving (Eq, Show)
data Piece = Piece Integer Integer BS.ByteString deriving (Eq, Show)

data PeerRPCParse = PeerRPCParse (Seq.Seq W.Word8) (Maybe BS.ByteString) [PeerRPC] deriving (Eq)

instance Show PeerRPCParse where
  show (PeerRPCParse word8s m rpcs) = "PeerRPCParse : " <> (show $ Seq.length word8s) <> " " <> show m <> " " <> show rpcs

type Work = [Block]
data Block = Block (Index Integer) (Begin Integer) (Length Integer) (SentTimestamp (Maybe Integer)) (SentCount Integer) (Maybe (Payload)) deriving (Eq, Show)

peerRPCToPiece :: PeerRPC -> Maybe Piece
peerRPCToPiece (Response pieceIndex blockOffset content) = Just $ Piece pieceIndex blockOffset content
peerRPCToPiece _                                         = Nothing

fmBlockToPeerWork :: Shared.BlockRequest -> Block
fmBlockToPeerWork (Shared.BlockRequest (Shared.PieceIndex i) (Shared.Begin b) (Shared.RequestLength rl)) = 
  Block (Index i) (Begin b) (Length rl) (SentTimestamp Nothing) (SentCount 0) Nothing

fmPeerWorkToBlock :: Block -> Shared.BlockRequest
fmPeerWorkToBlock (Block (Index i) (Begin b) (Length rl) _ _ _) =
  (Shared.BlockRequest (Shared.PieceIndex i) (Shared.Begin b) (Shared.RequestLength rl))

                               -- ((PeerChoked Bool), (InteresetedInPeer Bool))
                               -- ((PeerChoking Bool), (PeerInterested Bool))

-- clearWork :: FSMState -> FSMState
-- clearWork (FSMState id conn peer self wc rc rpc _ p lhb lka) =
--   (FSMState id conn peer self wc rc rpc [] p lhb lka)
-- data PeerState = PeerState { peerId :: BS.ByteString
--                            , peerPieceMap :: PieceMap
--                            , peerChokingMe :: Bool
--                            , peerInterestedInMe :: Bool
--                            }
--               deriving (Eq, Show)

-- data SelfState = SelfState { selfId :: BS.ByteString
--                            , selfPieceMap :: PieceMap
--                            , selfChokingPeer :: Bool
--                            , selfInterestedInPeer :: Bool
--                            }
--               deriving (Eq, Show)
updateFsmState :: FSMState -> PeerRPCParse -> Clock.TimeSpec -> FSMState
updateFsmState (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc work p lhb lka initiator) (PeerRPCParse w8 e peerRPCs) newKeepAliveTime = do
  let (PieceMap oldPieceMap) = peerPieceMap peer
  let bitfieldUpdatedPieceMap = maybe oldPieceMap (\new -> mergePieceMaps oldPieceMap new)
                                $ (\(BitField (PieceMap piecemap)) -> piecemap)
                                <$> find findBitField peerRPCs
  let pieceMap = foldr updatePieceWithHave bitfieldUpdatedPieceMap peerRPCs
  let newPeerChoking = foldl' updatePeerChoking (peerChokingMe peer) peerRPCs
  let newMaybeWork = mergeResponsesIntoWork (map (\(Response i b p) -> (i,b,p)) $ filter onlyResponses peerRPCs) work
  let newPeerInterested = (Interested ==) <$> (listToMaybe . reverse $ filter onlyInterestedOrNotInterested peerRPCs)
  let newPeer = PeerState (peerId peer) (PieceMap pieceMap) newPeerChoking (fromMaybe (peerInterestedInMe peer) newPeerInterested)
  let newPeerRPCs = filter clearConsumedRPCs peerRPCs
  FSMState id singleFileInfo pieceLength conn newPeer self wc rc (PeerRPCParse w8 e newPeerRPCs) newMaybeWork p lhb newKeepAliveTime initiator
  where
        clearConsumedRPCs (Have _)      = False
        clearConsumedRPCs (BitField _)  = False
        clearConsumedRPCs Choke         = False
        clearConsumedRPCs UnChoke       = False
        clearConsumedRPCs Interested    = False
        clearConsumedRPCs NotInterested = False
        clearConsumedRPCs Response{}    = False
        clearConsumedRPCs _             = True
        updatePieceWithHave (Have key) acc = fmap (\(k,v) -> if k == key then (k,True) else (k,v)) acc
        updatePieceWithHave _ acc = acc
        updatePeerChoking :: Bool -> PeerRPC -> Bool
        updatePeerChoking _ Choke  = True
        updatePeerChoking _ UnChoke = False
        updatePeerChoking acc _ = acc
        onlyResponses Response{} = True
        onlyResponses _ = False
        onlyInterestedOrNotInterested Interested    = True
        onlyInterestedOrNotInterested NotInterested = True
        onlyInterestedOrNotInterested _             = False
        mergeResponsesIntoWork :: [(Integer, Integer, BS.ByteString)] -> Work -> Work
        mergeResponsesIntoWork resonses work = foldr mergeResponsesIntoWorkFold work resonses
        mergeResponsesIntoWorkFold :: (Integer, Integer, BS.ByteString) -> Work -> Work
        mergeResponsesIntoWorkFold x = fmap (mergeResponsesIntoWorkMap x)
        mergeResponsesIntoWorkMap (index, begin, payload) block@(Block (Index i) (Begin b) l t c Nothing) =
          if index == i && begin == b then
            Block (Index i) (Begin b) l t c (Just (Payload payload))
          else
            block
        mergeResponsesIntoWorkMap _ block = block

mergePieceMaps :: [(a, Bool)] -> [(a, Bool)] -> [(a, Bool)]
mergePieceMaps = zipWith (\(x,xbool) (_,ybool) -> (x, xbool || ybool))

workIsEmpty :: FSMState -> Bool
workIsEmpty (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc [] p lhb lka initiator) = True
workIsEmpty _                                                   = False

replaceWork :: FSMState -> Work -> FSMState
replaceWork (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc _ p lhb lka initiator) work =
  (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc work p lhb lka initiator)

peerHasData :: FSMState -> Work -> Bool
peerHasData _ []                                      = False
peerHasData fSMState ((Block (Index p) _ _  _ _ _):_) =
  p `elem` (piecesThatCanBeDone $ peerPieceMap $ getPeer fSMState)

piecesThatCanBeDone :: PieceMap -> [Integer]
piecesThatCanBeDone (PieceMap pieceMap) = fmap fst $ filter (\(_, (k,v)) -> v) $ zip ([0..]) pieceMap

-- https://wiki.haskell.org/Data.List.Split
chunkWithDefault :: a -> Int -> [a] -> [[a]]
chunkWithDefault _ _ [] = []
chunkWithDefault d i xs =
  addDefault y : chunkWithDefault d i ys
  where (y, ys) = splitAt i xs
        addDefault z
          | length z == i = z
          | otherwise = z <> replicate (i - length z) d

pieceMapToBitField :: PieceMap -> BS.ByteString
pieceMapToBitField (PieceMap pieceMap) = do
  let chunks :: [[Bool]]
      chunks = (chunkWithDefault False 8 $ fmap snd pieceMap)
  let x = fmap (\(a:b:c:d:e:f:g:h:[]) -> Bitwise.packWord8BE a b c d e f g h) chunks
  let bitfield :: BS.ByteString
      bitfield = BS.pack x
      len = integerToBigEndian $ fromIntegral $ BS.length bitfield + 1
  BS.pack (len <> [5]) <> bitfield

maybeGetNewWork :: FSMState -> IO (Maybe Work)
maybeGetNewWork fsmState = do
  if workIsEmpty fsmState then do
    myLog fsmState "WORK IS EMPTY PULLING NEW WORK"
    loop 0
  else
    return Nothing
  where (PieceMap sPieceMap) = selfPieceMap $ getSelf fsmState
        oldWork = work fsmState
        maxNumberofPiecesLeft = length $ filter (not . snd) sPieceMap
        loop :: Int -> IO (Maybe Work)
        loop count = do
          myLog fsmState "going into loop"
          if (count >= maxNumberofPiecesLeft) then do
            myLog fsmState $ "looped through " <> (show count) <> " work chan messages before quitting"
            return Nothing
          else do
            maybeWork <- timeout 100 (Chan.readChan $ workChan fsmState)
            case maybeWork of
              Nothing -> do
                myLog fsmState $ "TRIED PULLING WORK BUT IT TIMED OUT"
                return Nothing
              Just (Shared.Work pi fmBlocks) -> do
                let work = fmBlockToPeerWork <$> fmBlocks

                myLog fsmState $ "pieces that can be done " <> (show $ piecesThatCanBeDone $ peerPieceMap $ getPeer fsmState)
                if peerHasData fsmState work then do
                  currentTime <- Clock.getTime Clock.Monotonic
                  Chan.writeChan (responseChan fsmState) (Shared.CheckOut (Shared.PeerThreadId  (fsmId fsmState)) pi currentTime)
                  return $ Just work
                else do
                  myLog fsmState $ "WORK CAN'T BE DONE, ON INDEX " <> (show pi) <> "BUT CAN BE DONE ON " <> (show $ piecesThatCanBeDone $ peerPieceMap $ getPeer fsmState) <> " SENDING BACK TO MANAGER AND TRYING AGAIN"
                  Chan.writeChan (responseChan fsmState) (Shared.Failed $ Shared.Work pi fmBlocks)
                  loop (count + 1)



tryToPullWork :: FSMState-> IO FSMState
tryToPullWork fsmState = do
  let oldWork = work fsmState
  maybeNewWork <- maybeGetNewWork fsmState
  myLog fsmState $ show maybeNewWork

  return $ maybe fsmState (replaceWork fsmState) maybeNewWork

  -- indicates that new work was pulled
  -- case maybeNewWork of
  --   Nothing ->
  --     return fsmState
  --   Just newWork -> 
  --     return $ replaceWork fsmState newWork

-- TODO: Create a property test to verify that for any block, completed, working, and worked states are mutually exclusive.
-- TODO: Maybe I should create a type to model block state
-- data BlockState = Unstarted | Working (TimeStamp, Count) | Complete BS.ByteString
filterCompletedBlocks :: Block -> Bool
filterCompletedBlocks (Block _ _ _ _ _ (Just _)) = True
filterCompletedBlocks _ = False

filterWorkingBlocks :: Block -> Bool
filterWorkingBlocks (Block _ _ _ (SentTimestamp (Just _)) _ Nothing) = True
filterWorkingBlocks _ = False

filterUnworkedBlocks :: Block -> Bool
filterUnworkedBlocks (Block _ _ _ (SentTimestamp Nothing) _ Nothing) = True
filterUnworkedBlocks _ = False

sendRequests :: FSMState -> IO FSMState
sendRequests fsmState = do
  let work = Peer.work fsmState
  let requestLimit = 10
  let completedRequests = filter filterCompletedBlocks work
  let workingRequests = filter filterWorkingBlocks work
  let unworkedRequests = filter filterUnworkedBlocks work
  let newRequestsToWork = take (requestLimit - (length workingRequests)) unworkedRequests
  let unworkedRequestsMinusNewRequestsToWork = drop (requestLimit - (length workingRequests)) unworkedRequests
  newWork <- traverse f newRequestsToWork
  let nextWork = sortOn (\(Block _ (Begin begin) _ _ _ _ ) -> begin) (newWork <>
                                                                      unworkedRequestsMinusNewRequestsToWork <>
                                                                      workingRequests <>
                                                                      completedRequests)
  -- print $ show a <>
  --         " work: " <> (show $ length work) <>
  --         " completedRequests: " <> (show $ length completedRequests) <> " " <> (show completedRequests) <>
  --         " workingRequests: " <> (show $ length workingRequests) <> " " <> (show workingRequests) <>
  --         " unworkedRequests: " <> (show $ length unworkedRequests) <>
  --         " newRequestsToWork: " <> (show $ length newRequestsToWork) <> " " <> (show newRequestsToWork) <>
  --         " unworkedRequestsMinusNewRequestsToWork: " <> (show $ length unworkedRequestsMinusNewRequestsToWork) <>
  --         " nextWork: " <> (show $ length nextWork) <>
  --         " currentHash: " <> (UTF8.toString $ shaHashRaw $ BS.concat (fmap (\(Block _ _ _ _ _ (Just (Payload payload))) -> payload) completedRequests))
  return $ replaceWork fsmState nextWork
  -- TODO: Currently we are not going to factor in time but this allows us to add it later if we would like to
  where
        f :: Block -> IO Block
        f (Block (Index pieceIndex) (Begin begin) (Length len) (SentTimestamp Nothing) (SentCount count) Nothing) = do
            -- print $ "SENDING: " <> (show $ (Block (Index pieceIndex) (Begin begin) (Length len) (SentTimestamp Nothing) (SentCount count) Nothing))
            let r = request pieceIndex begin len
            myLog fsmState $ " sending request " <> (show r)
            sendAll (getConn fsmState) r
            return $ Block (Index pieceIndex) (Begin begin) (Length len) (SentTimestamp $ Just 0) (SentCount (count + 1)) Nothing
        f x = return x

myLog :: FSMState -> String -> IO ()
myLog fsmState xs = return () -- do
  -- time <- Clock.getTime Clock.Monotonic
  -- putStrLn $ "THREAD " <> (UTF8.toString $ fsmId fsmState) <> " PEER " <> (UTF8.toString $ peerId $ getPeer fsmState) <> " AT " <> (show time) <> xs 
newKeepAlive :: Clock.TimeSpec -> Clock.TimeSpec -> Clock.TimeSpec
newKeepAlive now old =
  if 60 < (Clock.sec $ Clock.diffTimeSpec now old)
  then now
  else old

-- TODO Got to figure out how to send a keep alive to every peer every 30 seconds w/o blocking the thread
recvLoop :: FSMState -> IO ()
--recvLoop tracker peerState@(PeerState (PeerId peer_id) (Conn conn) pieceMap) peerRPCParse workChan responseChan currentWork = do
recvLoop fsmState = do -- @(PeerState (PeerId peer_id) (Conn conn) _ _ (RPCParse peerRPCParse) _ _ _ _ (TimeStamps (_, (LastKeepAlive lastKeepAlive))) pieces) = do
  -- TODO If anything throws, catch it, put the work back in the response queue for the parent thread
  --currentTime <- Clock.getCurrentTime
  let selfState = getSelf fsmState
  let conn = getConn fsmState
  myLog fsmState $ "Blocked on recvLoop, peerState: " <> (show fsmState)
  -- TODO If this is null but you still have checked out work, put the work back, I'm pretty sure I only need to do this on recv calls

  now <- Clock.getTime Clock.Monotonic
  let newlastKeepAlive = (newKeepAlive now $ lastKeepAlive fsmState)
  when (newlastKeepAlive == now) $ sendAll conn keepAlive

  msg <- recv conn 16384

  let newPeerRPCParse@(PeerRPCParse _ maybeErrors _) = parseRPC (selfPieceMap selfState) msg $ rpcParse fsmState
  myLog fsmState $ " newPeerRPCParse: " <> (show newPeerRPCParse)

  if (isJust maybeErrors) then do
    -- TODO: clear your work state and send it back to the parent
    myLog fsmState $ "ERROR: RECVLOOP hit parse error " <> (show newPeerRPCParse)
    _ <- sendWorkBackToManager fsmState
    return ()

  else
    if BS.null msg then do
      -- TODO: clear your work state and send it back to the parent
      myLog fsmState $ "DONE: RECVLOOP got null in receive "
      _ <- sendWorkBackToManager fsmState
      return ()
    else do
      let updatedFSMState = updateFsmState fsmState newPeerRPCParse newlastKeepAlive

      -- putStrLn $ "RECVLOOP after update: " <> show updatedFSMState
      if peerChokingMe $ getPeer updatedFSMState then do
        -- TODO: clear your work state and send it back to the parent
        myLog fsmState "Peer choking, can't ask it for pieces"
        updatedFSMStateWithoutWork <- sendWorkBackToManager updatedFSMState
                                        >>= buildPieces
                                        >>= sendPieces
        recvLoop updatedFSMStateWithoutWork-- newPeerRPCParse workChan responseChan currentWork
      else do
        myLog fsmState "Peer NOT choking, can potentially ask for pieces"
        finalPeerState <- sendFinishedWorkBackToManager updatedFSMState
                              >>= buildPieces
                              >>= sendPieces
                              >>= tryToPullWork
                              >>= sendRequests


        recvLoop finalPeerState-- newPeerRPCParse workChan responseChan currentWork

sendPieces :: FSMState -> IO FSMState
sendPieces fsmState = do --(FSMState a (Conn conn) c d (RPCParse (PeerRPCParse buffer err parsedRPCs)) f g h i j pieces) = do
  myLog fsmState $ "sending " <> (show $ length $ pieces fsmState) <> " pieces to peer " <> (show $ pieces fsmState)
  let bs = (pieceToBS <$> pieces fsmState)
  --
  -- let selfState = getSelf fsmState
  -- let newPeerRPCParse@(PeerRPCParse leftOver maybeErrors msgs) = parseRPC selfState (BS.concat bs) $ defaultPeerRPCParse
  --
  -- mapM_ t msgs
  mapM_ (sendAll $ getConn fsmState) bs
  return $ clearPieces fsmState
  --
  -- where t (Response x y content) = do
  --         let (PieceMap pm) = selfPieceMap $ getSelf fsmState
  --         myLog fsmState (show $ (shaHashRaw content) `elem` (fmap fst pm))

buildPieces :: FSMState -> IO FSMState
buildPieces fsmState@(FSMState fsmID singleFileInfo pieceLength conn peer self wc rc rpc work pieces lhb lka initiator) = do-- (PeerState a b c d (RPCParse (PeerRPCParse buffer err parsedRPCs)) f g h i j pieces) = do
  let (PeerRPCParse buffer err parsedRPCs) = rpcParse fsmState
  newPieces <- peerRPCsToPieces (getPieceLength fsmState) (getSingleFileInfo fsmState) parsedRPCs
  let newRPC = PeerRPCParse buffer err $ filter (not . isRequest) parsedRPCs
  return $ FSMState fsmID singleFileInfo pieceLength conn peer self wc rc newRPC work newPieces lhb lka initiator
  where isRequest :: PeerRPC -> Bool
        isRequest (Request _ _ _) = True
        isRequest _               = False

-- peerRPCToPieces rpcs = filter f rpcs
--   where f (Request )= 


pieceToBS :: Piece -> BS.ByteString
pieceToBS (Piece index offset content) =
  (BS.pack ((integerToBigEndian $ (fromIntegral $ BS.length content) + 9) <>
  [7] <>
  (integerToBigEndian $ fromIntegral index) <> (integerToBigEndian $ fromIntegral offset)))
  <> content


-- TODO: Need to check for whether the request is valid / safe
peerRPCsToPieces :: Integer -> (BS.ByteString, Integer) -> [PeerRPC] -> IO [Piece]
peerRPCsToPieces pieceLen (fileName, _fileLength) rpcs = do
  mapM (r $ UTF8.toString fileName) $ filter isRequest rpcs
  where r fileName (Request index begin len) = do
--          myLog fsmState $ "Request: " <> (show $ Request index begin len)
          fd <- PosixIO.openFd fileName PosixIO.ReadOnly Nothing PosixIO.defaultFileFlags
          readBS <- PosixIOBS.fdPread fd (fromIntegral len) (fromIntegral $ (pieceLen * index) + begin)
          PosixIO.closeFd fd
--          myLog fsmState $ "byte count " <> (show $ fromIntegral len)
--          myLog fsmState $ "offset " <> (show $ fromIntegral $ (pieceLength * index) + begin)
--          myLog fsmState $ "BS LENGTH " <> (show $ BS.length readBS)
          return $ Piece index begin readBS
        isRequest :: PeerRPC -> Bool
        isRequest (Request _ _ _) = True
        isRequest _         = False
-- import qualified System.Posix.IO as PosixIO
-- import qualified "unix-bytestring" System.Posix.IO.ByteString as PosixIOBS
-- import qualified System.Posix.Files.ByteString as PosixFilesBS
-- writePiece :: Tracker.Tracker -> SIO.FilePath -> PieceResponse -> IO ()
-- writePiece tracker filePath (PieceResponse (PieceIndex pieceIndex) (PieceContent content)) = do
--   fd <- PosixIO.openFd filePath PosixIO.WriteOnly Nothing PosixIO.defaultFileFlags
--   written <- PosixIOBS.fdPwrite fd content $ fromIntegral $ getTrackerPieceLength tracker * pieceIndex
--   PosixIO.closeFd fd
--   when (fromIntegral written /= BS.length content) $
--     E.throw $ userError "bad pwrite"

  -- TODO pull a piece from the channel, if your peer has that piece, workon it, otherwise put it back in the response channel
  -- While working, if the peer has a given piece, request that piece from them, starting by the offset, upon getting a response, continue to request blocks until you have the entire piece, then put the fullpiece on the response channel and pull a new bit of work.
clearWork :: FSMState -> FSMState
clearWork (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc _ p lhb lka initiator) =
  (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc [] p lhb lka initiator)

clearPieces :: FSMState -> FSMState
clearPieces (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc work pieces lhb lka initiator) =
  (FSMState id singleFileInfo pieceLength conn peer self wc rc rpc work [] lhb lka initiator)

sendWorkBackToManager :: FSMState -> IO FSMState
sendWorkBackToManager fsmState
  | work fsmState == [] = return fsmState
  | otherwise = do
    Chan.writeChan (responseChan fsmState) (Shared.Failed $ blockRequestsToWork blocks)
    return $ clearWork fsmState
  where blocks = fmPeerWorkToBlock <$> work fsmState
        blockRequestsToWork xs@(Shared.BlockRequest i _ _:_) = Shared.Work i xs

sendFinishedWorkBackToManager :: FSMState -> IO FSMState
sendFinishedWorkBackToManager fsmState = do
  let maybeWork = listToMaybe work
  if isJust maybeWork && isJust haveAllBlocks  then do
    let (Block (Index i) _ _ _ _ _) = fromJust maybeWork
    conforms <- conformsToHash (selfPieceMap $ getSelf fsmState) i $ fromJust haveAllBlocks
    case conforms of
      Just content -> do
        let success = Shared.Succeeded (Shared.PieceResponse (Shared.PieceIndex i) (Shared.PieceContent content))
        Chan.writeChan (responseChan fsmState) success
        return $ clearWork fsmState
      Nothing -> do
        -- DONE log and shed the broken state
        myLog fsmState "ERROR: not $ isJust haveAllBlocks && isJust maybeWork"
        sendWorkBackToManager fsmState
  else
    return fsmState
  where haveAllBlocks = traverse (\(Block _ _ _ _ _ maybePayload) -> maybePayload) $ sortOn (\(Block _ (Begin b) _ _ _ _) -> b) work
        work = Peer.work fsmState

conformsToHash :: PieceMap -> Integer -> [Payload] -> IO (Maybe BS.ByteString)
conformsToHash (PieceMap pieceMap) i payloads = do
  let (expectedSha,_) = pieceMap !! (fromIntegral i)
  let combinedPayload = BS.concat $ fmap (\(Payload b) -> b) payloads
  let sha = shaHashRaw combinedPayload
  -- print $ "CONFORMS TO HASH: Payload Length == " <> (show $ BS.length combinedPayload)
  -- print $ "CONFORMS TO HASH: expectedSha == " <> (UTF8.toString expectedSha)
  -- print $ "CONFORMS TO HASH: actual sha == " <> (UTF8.toString sha)
  return $ if expectedSha == sha then Just combinedPayload else Nothing

--[0,0,1,3,6,index,begin,length]
-- request: <len=0013><id=6><index><begin><length>

-- The request message is fixed length, and is used to request a block. The payload contains the following information:

--     index: integer specifying the zero-based piece index
--     begin: integer specifying the zero-based byte offset within the piece
--     length: integer specifying the requested length.

findBitField :: PeerRPC -> Bool
findBitField (BitField _) = True
findBitField _            = False

buildFSMState :: T.Tracker -> BS.ByteString -> BS.ByteString -> Socket -> Chan.Chan Shared.WorkMessage -> Chan.Chan Shared.ResponseMessage -> Clock.TimeSpec -> PieceMap -> Initiator -> FSMState
buildFSMState tracker fsmStateId peerId conn workC responseChan time pieceMap initiator =
  let selfState = SelfState (T.getTrackerPeerId tracker) pieceMap False True
      peerState = PeerState peerId (initPieceMap tracker) True False
      (T.SingleFileInfo (T.Name name) (T.Length len) _) = T.getTrackerSingleFileInfo tracker
      pieceLength = T.getTrackerPieceLength tracker
  in FSMState fsmStateId (name, len) pieceLength conn peerState selfState workC responseChan Peer.defaultPeerRPCParse [] [] Nothing time initiator

start :: T.Tracker -> Shared.Peer -> Chan.Chan Shared.WorkMessage -> Chan.Chan Shared.ResponseMessage -> Chan.Chan a -> PieceMap -> IO ()
start tracker peer@(Shared.Peer ip port) workC responseChan broadcastC pieceMap =  do
  putStrLn $ "Initiating handshake with " <> show peer
  maybePeerResponse <- initiateHandshake tracker peer
  putStrLn $ "Handshake result " <> show maybePeerResponse <>  " with " <> show peer
  unless (isNothing maybePeerResponse) $ do
    let (peerResponse@(PeerResponse _ (PeerId peer_id)), conn) = fromJust maybePeerResponse
    -- print $ "STARTPEER sending interested for: " <> show peer
    let bitMap = pieceMapToBitField pieceMap
    putStrLn $ "Sending pieceMap to peer " <> show peer <> " bitmap: " <> show bitMap <> "\nas well as interested & unchoke"
    sendAll conn bitMap
    sendAll conn interested
    sendAll conn unchoke
    time <- Clock.getTime Clock.Monotonic
  -- TODO I mistakenly didn't distinguish between the state of htorrent and the state of the peer, I should probably create 2 different structs to keep them straight
    let threadId = (ip <> (UTF8.fromString $ show port))
    let fsmState = buildFSMState tracker threadId peer_id conn workC responseChan time pieceMap Self
          -- TODO: You may need to close the connection if this fails, not sure of the consequences of this if I don't close it.
    myLog fsmState $ " Starting recvLoop"
    E.catch (recvLoop fsmState) (\e -> do
                                    myLog fsmState $ " HIT EXCEPTION " <> (show (e :: E.SomeException))
                                    E.throw e
                                )

defaultPeerRPCParse :: PeerRPCParse
defaultPeerRPCParse = PeerRPCParse Seq.empty Nothing []

initPieceMap :: T.Tracker -> PieceMap
initPieceMap t = PieceMap $ (\x -> (x, False)) <$> T.getTrackerPieces t

-- I can start parsing the bitfield so I can start trying to leach data

-- TODO This is going to need to be a
-- parseRPC :: BS.ByteString -> Run BS.ByteString  (Maybe [PeerRPC])
-- B/c you might not yet have all the data you need and multiple messages can be received in a given receive block
-- there are also situations where you might want to drop the connection if the parsing rules are violated
-- Right now this makes the assumption that we are only ever getting a single RPC in a given recv call (which is not true)

bigEndianToInteger :: [Binary.Word8] -> Maybe Binary.Word32
bigEndianToInteger xs =
  if length xs == 4 then
    Just $ Binary.decode $ Lazy.fromStrict $ BS.pack xs
  else
    Nothing

maxIntInByteSize :: Int -> Integer
maxIntInByteSize byteSize = foldr (\_ acc -> 256 * acc) 1 [0..(byteSize-1)]  - 1

integerToBigEndian :: Binary.Word32 -> [W.Word8]
integerToBigEndian = BS.unpack .Lazy.toStrict . Binary.encode

parseRPC :: PieceMap -> BS.ByteString -> PeerRPCParse -> PeerRPCParse
parseRPC pieceMap bs peerRPCParse =
  BS.foldl (parseRPC' pieceMap) peerRPCParse bs

parseRPC' :: PieceMap -> PeerRPCParse -> W.Word8 -> PeerRPCParse
parseRPC' (PieceMap pieceMap) (PeerRPCParse word8Buffer Nothing xs) word8
  | newBuffer == (Seq.fromList [0,0,0,0]) = PeerRPCParse (Seq.empty) Nothing (xs <> [PeerKeepAlive])
  | newBuffer == (Seq.fromList [0,0,0,1,0]) = PeerRPCParse (Seq.empty) Nothing (xs <> [Choke])
  | newBuffer == (Seq.fromList [0,0,0,1,1]) = PeerRPCParse (Seq.empty) Nothing (xs <> [UnChoke])
  | newBuffer == (Seq.fromList [0,0,0,1,2]) = PeerRPCParse (Seq.empty) Nothing (xs <> [Interested])
  | newBuffer == (Seq.fromList [0,0,0,1,3]) = PeerRPCParse (Seq.empty) Nothing (xs <> [NotInterested])
  | Seq.take 5 newBuffer == (Seq.fromList [0,0,0,5,4]) =
    if length newBuffer == 9 then
      PeerRPCParse (Seq.drop 9 newBuffer) Nothing (xs <> [Have $ BS.pack $ toList $ Seq.take 4 $ Seq.drop 5 newBuffer])  --NOTE: I think that this maybe should be (Have $ BS.pack $ take 1 $ drop 5 unpackedMsg)
    else
      PeerRPCParse newBuffer Nothing xs  --NOTE: I think that this maybe should be (Have $ BS.pack $ take 1 $ drop 5 unpackedMsg)
  | Seq.drop 4 (Seq.take 5 newBuffer) == Seq.singleton 5  = do
--                                            let bitfieldLength =  fromIntegral ((newBuffer !! 3) - 1)
                                            let bitfieldLength =  fromIntegral (fromJust $ bigEndianToInteger $ toList $ Seq.take 4 newBuffer) - 1
                                            let word8s = Seq.take bitfieldLength (Seq.drop 5 newBuffer)

                                            if (ceiling ((fromIntegral . length $ pieceMap) / 8)) /= bitfieldLength then
                                              PeerRPCParse newBuffer (Just "ERROR parseRPC in BitField parse, (ceiling ((fromIntegral . length $ T.getTrackerPieces tracker) / 8)) /= bitfieldLength") xs
                                            else do
                                              --if (S.size (S.fromList (T.getTrackerPieces tracker)) /= (length (T.getTrackerPieces tracker))) then
                                               -- PeerRPCParse newBuffer (Just "ERROR parseRPC in BitField parse, (S.size (S.fromList (T.getTrackerPieces tracker)) /= (length (T.getTrackerPieces tracker)))") xs
                                              --else do
                                              -- If the number of bools are lower than the number of pieces then we don't have enough data to proceed
                                              if length word8s /= bitfieldLength then do
                                                PeerRPCParse newBuffer Nothing xs
                                              else do
                                                let boolsBeforeCheck = word8s >>= (\ x -> Seq.fromList $ reverse [Bits.testBit x i | i <- [0 .. 7]])
                                                let extraBits = Seq.drop (length pieceMap) boolsBeforeCheck

                                                -- If we have any extra bits we should  drop the connection
                                                if or extraBits then
                                                  PeerRPCParse newBuffer (Just "ERROR parseRPC in BitField parse, extra bits are set") xs
                                                else
                                                  PeerRPCParse (Seq.drop (bitfieldLength + 5) newBuffer) Nothing (xs <> [BitField $ PieceMap $ zip (fmap fst pieceMap) (toList boolsBeforeCheck)])
  | Seq.take 5 newBuffer == (Seq.fromList [0,0,0,13,6]) = if length newBuffer >= 17 then
                                          PeerRPCParse Seq.empty Nothing (xs <> [Request (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 5 newBuffer) (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 9 newBuffer) (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 13 newBuffer)])
                                        else
                                          PeerRPCParse newBuffer Nothing xs
  | Seq.drop 4 (Seq.take 5 newBuffer) == (Seq.singleton 7) = do
    let blockLen = fromIntegral $ (fromJust $ bigEndianToInteger $ toList $ Seq.take 4 newBuffer) - 9
    let blockWord8s = Seq.take blockLen $ Seq.drop 13 newBuffer
    let indexWord8s = Seq.take 4 $ Seq.drop 5 newBuffer
    let beginWord8s = Seq.take 4 $ Seq.drop 9 newBuffer

    if length blockWord8s /= blockLen then
      PeerRPCParse newBuffer Nothing xs
    else do
      let index = fromIntegral $ fromJust $ bigEndianToInteger $ toList indexWord8s
      let begin = fromIntegral $ fromJust $ bigEndianToInteger $ toList beginWord8s
      let block = BS.pack $ toList blockWord8s
      PeerRPCParse (Seq.drop (5 + 4 + 4 + blockLen) newBuffer) Nothing (xs <> [Response index begin block])
  | Seq.take 5 newBuffer == (Seq.fromList [0,0,0,13,8]) = if length newBuffer >= 17 then
                                          PeerRPCParse Seq.empty Nothing (xs <> [Cancel (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 5 newBuffer) (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 9 newBuffer) (fromIntegral $ fromJust $ bigEndianToInteger $ toList $ Seq.take 4 $ Seq.drop 13 newBuffer)])
                                        else
                                          PeerRPCParse newBuffer Nothing xs
  | otherwise = PeerRPCParse newBuffer Nothing xs
  where newBuffer = word8Buffer Seq.|> word8
parseRPC' _ (PeerRPCParse word8Buffer error xs) word8 = PeerRPCParse newBuffer error xs
  where newBuffer = word8Buffer Seq.|> word8


keepAlive :: BS.ByteString
keepAlive = BS.pack [0,0,0,0]

choke :: BS.ByteString
choke = BS.pack [0,0,0,1,0]

unchoke :: BS.ByteString
unchoke = BS.pack [0,0,0,1,1]

interested :: BS.ByteString
interested = BS.pack [0,0,0,1,2]

notInterested :: BS.ByteString
notInterested = BS.pack [0,0,0,1,3]

have :: BS.ByteString -> BS.ByteString
have pieceIndex = BS.concat [BS.pack [0,0,0,5,4], pieceIndex]

isValidBitField :: BS.ByteString -> Bool
isValidBitField bs = len == fromIntegral (length $ drop 5 xs)
  where xs = BS.unpack bs
        len = xs !! 3 - 1

request :: Integer -> Integer -> Integer -> BS.ByteString
request index begin len =  BS.pack  $ ([0,0,0,13,6]) <> (integerToBigEndian $ fromIntegral index)
                                                                 <> (integerToBigEndian $ fromIntegral begin)
                                                                 <> (integerToBigEndian $ fromIntegral len)

trackerToPeerHandshake :: T.Tracker -> BS.ByteString
trackerToPeerHandshake (T.Tracker (T.PeerId peer_id) _ _ _ (T.InfoHash info_hash) _ _ _) =
  handshake info_hash peer_id

initiateHandshake :: T.Tracker -> Shared.Peer -> IO (Maybe (PeerResponse, Socket))
initiateHandshake tracker peer = do
  (response, conn) <- sendHandshake peer $ trackerToPeerHandshake tracker
  let validated = readHandShake response >>= validateHandshake tracker
  case validated of
    Nothing -> do
      close conn
      return Nothing
    Just x ->
      return $ Just (x, conn)

handshake :: BS.ByteString -> BS.ByteString -> BS.ByteString
handshake info_hash peer_id =  BS.concat [pstrlen, pstr, reserved, unhex info_hash, peer_id]
  where pstr = "BitTorrent protocol"
        pstrlen = BS.singleton $ fromIntegral (BS.length pstr)
        reserved = BS.replicate 8 W._nul


-- TODO: If the peer_id differs from what the tracker sent (if the tracker sent
-- anything regarding the peer_id) then it should be dropped.
-- TODO try to send 16k blocks
-- Try downloading pieces in random order first, then, as an optimization, try rairest first.
-- Only change choaked peers once every 10 seconds (to prevent fibralation)
readHandShake :: BS.ByteString  ->  Maybe PeerResponse
readHandShake r = PeerResponse <$> (InfoHash <$> infoHash) <*> (PeerId <$> peerId)
  where word8s = BS.unpack r
        afterProtocol :: Maybe BS.ByteString
        afterProtocol = BS.pack . flip drop word8s . (1+) . fromIntegral <$> listToMaybe word8s
        afterEmpty :: Maybe BS.ByteString
        afterEmpty = BS.drop 8 <$> afterProtocol
        infoHash :: Maybe BS.ByteString
        infoHash = BS.take 20 <$> afterEmpty
        peerId :: Maybe BS.ByteString
        peerId = BS.drop 20 <$> afterEmpty

validateHandshake :: T.Tracker -> PeerResponse -> Maybe PeerResponse
validateHandshake tracker@(T.Tracker _ _ _ _ (T.InfoHash info_hash) _ _ _)
                  pr@(PeerResponse (InfoHash peerInfoHash) (PeerId peerID)) =
                  if unhex info_hash == peerInfoHash && (T.getTrackerPeerId tracker /= peerID)then
                    Just pr
                  else
                    Nothing


-- This can return whatever you want (I believe including the socket)
sendHandshake :: Shared.Peer -> BS.ByteString -> IO (BS.ByteString, Socket)
sendHandshake (Shared.Peer ip port) bs = do
  --print "sending"
  sock <- addrIO >>= open
  sendAll sock bs
  --print $ BS.concat ["sent: ", bs]
  -- This is to make it so that I don't overfetch here, given that I know
  -- exactly how many bytes I need to read in for the handshake response.
  -- 49 + (length "BitTorrent protocol") == 58
  msg <- recv sock 68
  -- getSocketOption sock RecvTimeOut >>= (\x -> print $ "RecvTimeOut : " <> (show x))
  return (msg, sock)

  where hints = defaultHints { addrSocketType = Stream }
        addrIO = head <$> getAddrInfo (Just hints) (Just $ UTF8.toString ip) (Just $ show port)
        open addr = do
          -- TODO Set recv timeout https://hackage.haskell.org/package/network-2.7.0.2/docs/Network-Socket.html 
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          connect sock $ addrAddress addr
          -- print "in open addr"
          -- print "addr"
          -- print addr
          -- print "sock"
          -- print sock
          return sock
