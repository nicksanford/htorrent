module Shared where
import qualified Data.ByteString           as BS
import qualified System.Clock as Clock

newtype PieceIndex a = PieceIndex a deriving (Eq, Show, Ord)
newtype Begin a = Begin a deriving (Eq, Show, Ord)
newtype RequestLength a = RequestLength a deriving (Eq, Show, Ord)
newtype PieceContent a = PieceContent a deriving (Eq, Show, Ord)
newtype PieceSha a = PieceSha a deriving (Eq, Show, Ord)
newtype PeerThreadId a = PeerThreadId a deriving (Eq, Show, Ord)

data BlockRequest = BlockRequest (PieceIndex Integer) (Begin Integer) (RequestLength Integer) deriving (Eq, Show, Ord)

data PieceResponse = PieceResponse (PieceIndex Integer) (PieceContent BS.ByteString) deriving (Eq, Show, Ord)

data WorkMessage = Work (PieceIndex Integer) [BlockRequest] deriving (Eq, Show)

data ResponseMessage = Failed WorkMessage
                     | Succeeded PieceResponse
                     | Error Peer
                     | HeartBeat(PeerThreadId BS.ByteString) (PieceIndex Integer)
                     | CheckOut (PeerThreadId BS.ByteString) (PieceIndex Integer) Clock.TimeSpec
                     | CheckPeers
                     | CheckWork
                     deriving (Eq, Show)


type IP = BS.ByteString
type Port = Integer
data Peer = Peer IP Port deriving (Eq, Show)

blockSize :: Integer
blockSize = 2^14 -- 16k
