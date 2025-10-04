{-# LANGUAGE DeriveGeneric #-}
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
  , getProtocolVersion

  -- ** Client builder
  , newClient

  -- * Torrent reference
  , TorrentRef (..)

  , Label
  , RPCMethod(..)

  , JSONTypes (..)
  , Args (Args)

  , ID(..)
  , IDs(..)

 )
where

import           Data.Aeson              (FromJSON, ToJSON, toJSON)
import           Data.ByteString         (ByteString)
import           Effectful.FileSystem.IO (Handle)
import           Effectful.Wreq          (Options)
import           Effectful.Wreq.Session  (Session)
import           GHC.Generics            (Generic)

data Client = Client {
                       getURI             :: URI
                     , getSession         :: Session
                     , getOpts            :: Options
                     , getProtocolVersion :: Int
                     }

data Scheme = HTTP | HTTPS deriving (Show, Read)

data TorrentRef = TorrentURI URI | Binary Handle | TorrentContent ByteString | Path FilePath deriving Show

data TorrentObject = TorrentObject deriving Show

data ID = ID Int | Hash String deriving (Show, Generic)
data IDs = IDs [ID] | RecentlyActive deriving (Show, Generic)

type URI = String
type Username = Maybe String
type Password = Maybe String
type Host = String
type Port = Int
type Path = String
type Timeout = Maybe Int
type Label = String

data RPCMethod = TorrentAdd | TorrentGet | TorrentReannounce | TorrentRemove | TorrentSet | TorrentStart | TorrentStartNow | TorrentStop | TorrentVerify | SessionGet | SessionStats | PortTest | BlocklistUpdate | FreeSpace | TorrentRenamePath deriving Show

data JSONTypes = JSONNumber | JSONDouble | JSONObject | JSONString | JSONArray | JSONBool deriving Show

data Args = Args JSONTypes Int (Maybe Int) (Maybe String) (Maybe String) String deriving Show

instance ToJSON RPCMethod where
  toJSON TorrentAdd        = toJSON "torrent-add"
  toJSON TorrentGet        = toJSON "torrent-get"
  toJSON TorrentReannounce = toJSON "torrent-reannounce"
  toJSON TorrentRemove     = toJSON "torrent-remove"
  toJSON TorrentSet        = toJSON "torrent-set"
  toJSON TorrentStart      = toJSON "torrent-start"
  toJSON TorrentStartNow   = toJSON "torrent-start-now"
  toJSON TorrentStop       = toJSON "torrent-stop"
  toJSON TorrentVerify     = toJSON "torrent-verify"
  toJSON SessionGet        = toJSON "session-get"
  toJSON SessionStats      = toJSON "session-stats"
  toJSON PortTest          = toJSON "port-test"
  toJSON BlocklistUpdate   = toJSON "blocklist-update"
  toJSON FreeSpace         = toJSON "free-space"
  toJSON TorrentRenamePath = toJSON "torrent-rename-path"

instance ToJSON ID where
  toJSON (ID i)   = toJSON i
  toJSON (Hash s) = toJSON s

instance FromJSON ID

instance ToJSON IDs where
  toJSON (IDs ids)      = toJSON ids
  toJSON RecentlyActive = toJSON "recently-active"

instance FromJSON IDs

newClient :: URI -> Session -> Options -> Int -> Client
newClient = Client
