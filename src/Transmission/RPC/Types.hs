module Transmission.RPC.Types
  (
  -- * Client

  Client ()
  , Scheme (..)
  , Username
  , Password
  , Host
  , Port
  , Path
  , Timeout

  -- ** Client getter methods
  , getURI
  , getSession
  , getOpts

  -- ** Client builder
  , newClient

  -- * Torrent reference
  , TorrentRef (..)
  , ID

  , Label
  , RPCMethod(..)

  , Torrent

 )
where

import           Data.Aeson              (ToJSON, Value, toJSON)
import           Data.Aeson.KeyMap       (KeyMap)
import           Data.ByteString         (ByteString)
import           Effectful.FileSystem.IO (Handle)
import           Effectful.Wreq          (Options)
import           Effectful.Wreq.Session  (Session)

data Client = Client {
                       getURI     :: URI
                     , getSession :: Session
                     , getOpts    :: Options
                     }

data Scheme = HTTP | HTTPS deriving (Show, Read)

data TorrentRef = TorrentURI URI | Binary Handle | TorrentContent ByteString | Path FilePath deriving Show

data TorrentObject = TorrentObject deriving Show

type URI = String
type Username = Maybe String
type Password = Maybe String
type Host = String
type Port = Int
type Path = String
type Timeout = Maybe Int
type Label = String
type ID = Int

data RPCMethod = TorrentAdd | TorrentGet | SessionGet | SessionStats | PortTest | BlocklistUpdate | FreeSpace | TorrentRenamePath deriving Show

type Torrent = KeyMap Value

instance ToJSON RPCMethod where
  toJSON TorrentAdd        = toJSON "torrent-add"
  toJSON TorrentGet        = toJSON "torrent-get"
  toJSON SessionGet        = toJSON "session-get"
  toJSON SessionStats      = toJSON "session-stats"
  toJSON PortTest          = toJSON "port-test"
  toJSON BlocklistUpdate   = toJSON "blocklist-update"
  toJSON FreeSpace         = toJSON "free-space"
  toJSON TorrentRenamePath = toJSON "torrent-rename-path"

newClient :: URI -> Session -> Options -> Client
newClient = Client
