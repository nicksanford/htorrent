# htorrent
[![made at Recurse Center](https://cdn.rawgit.com/heatherbooker/made_at_rc/master/made_at_RC.svg)](https://www.recurse.com)

HTorrent is a work-in-progress BitTorrent client written in Haskell. This was built as a learning exercise. It is not intended for use in production.

## Installation:
NOTE: HTorrent uses pwrite & pread under the hood to allow for parallel reads of tracker content. For this reason, only POSIX system are supported (sorry windows).

### Binary Install
### Stack install


## Goals:
I built this as an exercise to learn Haskell (which had only minimal exposure to prior to this project) and use it to build a non trivial project from scratch.
I also wanted to learn about parsing, bit manipulation, socket programming, property based testing, how to build efficient concurrent / parallel programs in Haskell, and how to profile & optimize Haskell programs.


## Features
### Implemented:
- [x] Able to resume partial download with no duplicate work
- [x] Accepting incoming connections from peers
- [x] Computes & sends initial bitfield message to peers
- [x] Hash checking to verify that each piece received is valid
- [x] Is able to seed & leach from itself
- [x] Never downloads the same piece twice.
- [x] Only requests a piece from a peer if it already knows the peer has the piece.
- [x] Parallel leaching & seeding
- [x] Property & unit tests for parsing logic
- [x] Reading single torrent
- [x] Tracks if peer is choking and does not request pieces if peer is choking
- [x] Tracks which pieces each peer has by parsing have & bitfield messages.

### TODO (next to be implemented):
- Fix seeding issue. Issue manifests if 2 HTorrent clients are running on different computers, A with all the file, and B with none of the file. If A connects first, B will not make forward progress. I think the problem is that B's call to A never works b/c A's TCP server is bound to the loopback interface.
- Publishing have messages to all peers as pieces complete
- Support for large single files (currently hits an out of memory error when trying to download 10 GB file)
- Profiling to see the cause of high CPU & memory usage
- Refactor FSM logic, which is the same regardless of whether connection is initiated by peer or HTorrent, out of Peer module & into an FSM module. During this step, also refactor out any test code outside of the test directory.
- Add CI/CD

### Not (yet?) Implemented:
- Asking for rarest blocks first.
- Change RPC parser to use Pttoparsec
- Close connections with peers when you have everything and they have everything
- Convert cabal file into an hpack file
- DHT support
- Endgame - asking for the same pieces from multiple peers when number of outstanding pieces is lower than number of peers which can serve them.
- Game theory algorithm for deciding when to choke peers (currently peers are always unchoked)
- Only sending interested messages when the peer actually has pieces HTorrent needs (currently HTorrent is always interested in all peers)
- Proper CLI with user feedback, currently just gets trackerfile path & TCP port for TCP server and exits if they are not provided.
- Property / integration tests for bittorrent FSM itself
- Property tests for RPC parser
- Provide ability to get debug logging
- Provide option to exit on completion
- Provide visualization (presentation mode)
- Requesting tracker for more peers when HTorrent runs low on peers
- Resolve all compiler warnings
- Signal handling for Ctrl-C & clean shutdown
- Support for WebSeeds http://techie-buzz.com/how-to/what-are-web-seeds-bittorrent.html
- Support for multiple torrents
- Support for multiple trackers
- Support for specifying the download directory (currently it just downloads the file in the current directory)
- Torrents which contain more than one file
- Track download statistics to report back to tracker
- Tracking & reporting total download & upload rate
- UDP Tracker Protocol

## Contributions:
- PRs welcome! If you would like to add/suggest a feature, please feel free to submit a PR/issue.
