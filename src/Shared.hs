{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Shared where

import Network.Socket
import Control.DeepSeq 
import GHC.Generics (Generic)
import qualified Control.Concurrent.Chan as Chan
import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as UTF8
import qualified Data.Sequence             as Seq
import qualified Data.Word8                as W
import qualified System.Clock as Clock
import qualified Data.List.NonEmpty as NonEmptyL

-- TODO - It might be a good idea to unify the multiple types of blocks into a single type.
data BlockRequest = BlockRequest { bIndex             :: Integer
                                 , bBegin             :: Integer
                                 , bLength            :: Integer
                                 , bInitiator         :: Initiator
                                 , bSentCount         :: Integer
                                 , bPayload           :: Maybe Payload
                                 } deriving (Eq, Show, Generic, NFData, Ord)

data PieceRequest = PieceRequest { preqIndex :: Integer
                                 , preqBlockRequests :: NonEmptyL.NonEmpty BlockRequest
                                 } deriving (Eq, Show)

data PieceResponse = PieceResponse { presIndex :: Integer
                                   , piecePayload :: BS.ByteString
                                   } deriving (Eq, Show)

data BlockResponse = BlockResponse { pIndex :: Integer
                                   , pBegin :: Integer
                                   , pBlock :: Payload
                                   } deriving (Eq, Show, Generic, NFData)

data ResponseMessage = Failed PieceRequest
                     | Succeeded PieceResponse
                     | Error Peer
                     | HeartBeat PeerThreadId PieceIndex
                     | CheckOut PeerThreadId PieceIndex Clock.TimeSpec
                     | CheckPeers
                     | CheckWork
                     deriving (Eq, Show)


data FSMState = FSMState { fsmId             :: BS.ByteString
                         , getTracker        :: Tracker
                         , getConn           :: Socket
                         , getPeer           :: PeerState
                         , getSelf           :: SelfState
                         , workChan          :: Chan.Chan PieceRequest
                         , responseChan      :: Chan.Chan ResponseMessage
                         , rpcParse          :: PeerRPCParse
                         , initiator         :: Initiator
                         }
                         deriving (Eq)

data Initiator = SelfInitiated
               | PeerInitiated
               deriving (Eq, Show, Generic, NFData, Ord)

data Peer = Peer { pIP :: BS.ByteString
                 , pPort :: Integer
                 } deriving (Eq, Show)

data PeerResponse = PeerResponse { prInfoHash :: BS.ByteString
                                 , prPeerId :: BS.ByteString
                                 } deriving (Eq, Show, Generic, NFData)


data PeerRPCParse = PeerRPCParse { pRPCUnparsed :: Seq.Seq W.Word8
                                 , pRPCError :: Maybe BS.ByteString
                                 , pRPCParsed :: [PeerRPC]
                                 } deriving (Eq, Generic, NFData)

data PeerRPC = PeerKeepAlive
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have BS.ByteString
             | BitField PieceMap
             | Cancel Integer Integer Integer
             | Request BlockRequest
             | Response BlockResponse
             deriving (Eq, Show, Generic, NFData)

data PeerState = PeerState { peerId                   :: BS.ByteString
                           , peerPieceMap             :: PieceMap
                           , peerPieceRequestFromSelf :: Maybe PieceRequest
                           , blockResponsesForPeer    :: [BlockResponse]
                           , peerChokingMe            :: Bool
                           , peerInterestedInMe       :: Bool
                           , lastHeartBeat            :: Maybe Clock.TimeSpec
                           , lastKeepAlive            :: Clock.TimeSpec
                           }
              deriving (Eq)

instance Show PeerState where
  show a = "PeerState { " <> "peerId = " <> UTF8.toString (peerId a) <> ", "
                          <> "peerPieceMap = " <> showPeerPieceMap (peerPieceMap a) <> ", "
                          <> "peerPieceRequestFromSelf = " <> showPieceRequest (peerPieceRequestFromSelf a) <> ", "
                          <> "blockResponsesForPeer = " <> show (blockResponsesForPeer a) <> ", "
                          <> "peerChokingMe = " <> show (peerChokingMe a) <> ", "
                          <> "peerInterestedInMe = " <> show (peerInterestedInMe a) <> ", "
                          <> "lastHeartBeat = " <> show (lastHeartBeat a) <> ", "
                          <> "lastKeepAlive = " <> show (lastKeepAlive a) <> ", "
                          <> "}"

showPeerPieceMap :: PieceMap -> String
showPeerPieceMap m = "PieceMap len: " <> (show $ length m) <> "number of set bits: " <> (show $ length $ filter snd m)

showPieceRequest :: Maybe PieceRequest -> String
showPieceRequest Nothing = "Nothing"
showPieceRequest (Just pr) = "PieceRequest { " <> "preqIndex = " <> show (preqIndex pr) <> " preqBlockRequests = " <> (show $ NonEmptyL.length $ preqBlockRequests pr ) <> " }"

-- TODO refactor this to have an instance of functor on PieceRequest, preqIndex should be part of the functoral structure and preqBlockRequests should be what is fmapped over
data SelfState = SelfState { selfId                   :: BS.ByteString
                           , selfPieceMap             :: PieceMap
                           , selfChokingPeer          :: Bool
                           , selfInterestedInPeer     :: Bool
                           } deriving (Eq)

instance Show SelfState where
  show a = "SelfState { " <> "selfId = " <> (UTF8.toString $ selfId a) <> ",\n"
                          <> "selfPieceMap = " <> (showPeerPieceMap $ selfPieceMap a) <> ",\n"
                          <> "selfChokingPeer = " <> (show $ selfChokingPeer a) <> ",\n"
                          <> "selfInterestedInPeer = " <> (show $ selfInterestedInPeer a)
                          <> "}"

-- Tracker Related
data SingleFileInfo = SingleFileInfo { sfName        :: BS.ByteString
                                     , sfLength      :: Integer
                                     , sfMaybeMD5Sum :: Maybe BS.ByteString
                                     } deriving (Eq, Show)

data DirectoryInfo = DirectoryInfo { diName  :: BS.ByteString
                                   , diFiles :: [SingleFileInfo]
                                   } deriving (Eq, Show)

-- TODO: You will need to implement a Maybe PeerId here as it is possible for that to be provided. If it is, then you need to verify that the peer id you get back from the handshake is the same as what the tracker said.

data Tracker = Tracker { tPeerId             :: BS.ByteString
                       , tAnnounce           :: BS.ByteString
                       , tPieceLength        :: Integer
                       , tPieceHashes        :: [BS.ByteString]
                       , tInfoHash           :: BS.ByteString
                       , tSingleFileInfo     :: SingleFileInfo
                       , tMaybeDirectoryInfo :: Maybe DirectoryInfo
                       , tMaybeEncoding      :: Maybe BS.ByteString
                       } deriving (Eq, Show)

data TrackerResponse = TrackerResponse { trPeers            :: [Peer]
                                       , trInterval         :: Integer
                                       , trMaybeTrackerId   :: Maybe BS.ByteString
                                       , trMaybeWarning     :: Maybe BS.ByteString
                                       , trMaybeMinInterval :: Maybe Integer
                                       , trMaybeComplete    :: Maybe BS.ByteString
                                       , trMaybeInComplete  :: Maybe BS.ByteString
                                       } deriving (Eq, Show)

type Payload = BS.ByteString
newtype Chans a = Chans a deriving (Eq)

type PeerThreadId = BS.ByteString
type PieceIndex = Integer

type PieceMap = [(BS.ByteString, Bool)]
-- instance Show PieceMap where
--   show (PieceMap m) = "PieceMap len: " <> (show $ length m) <> "number of set bits: " <> (show $ length $ filter snd m)

instance Show (Chans m) where
  show (Chans _) = "Chans"

-- instance Show Payload where
--   show (Payload m) = "Payload length: " <> (show $ BS.length m)

instance Show FSMState where
  show a = "FSMState { fsmId: "          <> (show $ fsmId a) <> ",\n"
                   <> "getConn: "        <> (show $ getConn a) <> ",\n"
                   <> "getPeer: "        <> (show $ getPeer a) <> ",\n"
                   <> "getSelf: "        <> (show $ getSelf a) <> ",\n"
                   <> "rpcParse: "       <> (show $ rpcParse a) <> ",\n"
                   <> "initiator: "       <> (show $ initiator a)
                   <> "}"

instance Show PeerRPCParse where
  show (PeerRPCParse word8s m rpcs) = "PeerRPCParse : " <> (show $ Seq.length word8s) <> " " <> show m <> " " <> show rpcs

blockSize :: Integer
blockSize = (2::Integer)^(14::Integer) -- 16k

myLog :: FSMState -> String -> IO ()
myLog fsmState msg = do
  now <- Clock.getTime Clock.Monotonic
  putStrLn $ (show now) <> "\n" <> msg <> "\n" <> (show fsmState) <> "\n\n"
