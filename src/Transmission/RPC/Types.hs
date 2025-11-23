{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Transmission.RPC.Types
  (
  -- * Client

  Username
  , Password
  , Host
  , Port
  , Path
  , Timeout

  -- * Torrent reference
  , TorrentRef (..)

  , Label
  , RPCMethod(..)

  , JSONTypes (..)
  , Args (Args)

  , ID(..)
  , IDs(..)

  , Error(..)

  , ETA(..)

  , Status(..)
  , URI
  , ClientRep (ClientRep)
  , uri
  , session
  , protocolVersion
  , serverVersion
  , semVerVersion
 )
where

import           Data.Aeson               (FromJSON, ToJSON, Value (Number),
                                           toJSON, withScientific)
import           Data.Aeson.Types         (FromJSON (parseJSON), prependFailure)
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import           Data.Time                (NominalDiffTime)
import           Effectful.FileSystem.IO  (Handle)
import           GHC.Generics             (Generic)
import           Lens.Micro.TH            (makeLenses)
import           Transmission.RPC.Session (Session)

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
type Label = Text

data ClientRep = ClientRep {
               _uri             :: URI,
               _session         :: Session,
               _protocolVersion :: Int,
               _serverVersion   :: Maybe Text,
               _semVerVersion   :: Maybe Text}

makeLenses ''ClientRep

data RPCMethod = GroupSet | GroupGet | QueueMoveBottom | QueueMoveDown | QueueMoveTop | QueueMoveUp | TorrentAdd | TorrentGet | TorrentReannounce | TorrentRemove | TorrentSetLocation | TorrentSet | TorrentStart | TorrentStartNow | TorrentStop | TorrentVerify | SessionGet | SessionStats | PortTest | BlocklistUpdate | FreeSpace | TorrentRenamePath deriving Show

data JSONTypes = JSONNumber | JSONDouble | JSONObject | JSONString | JSONArray | JSONBool deriving Show

data Args = Args JSONTypes Int (Maybe Int) (Maybe String) (Maybe String) String deriving Show

data Error = OK | Warning | Tracker | Local deriving (Show, Eq)

data ETA where
  ETA :: NominalDiffTime -> ETA
  NA :: ETA
  Unknown :: ETA
  deriving (Show, Eq, Ord)

data Status = Stopped | CheckPending | Checking | DownloadPending | Downloading | SeedPending | Seeding deriving (Show, Eq)

instance ToJSON RPCMethod where
  toJSON GroupSet           = "group-set"
  toJSON GroupGet           = "group-get"
  toJSON QueueMoveBottom    = "queue-move-bottom"
  toJSON QueueMoveDown      = "queue-move-down"
  toJSON QueueMoveTop       = "queue-move-top"
  toJSON QueueMoveUp        = "queue-move-up"
  toJSON TorrentAdd         = "torrent-add"
  toJSON TorrentGet         = "torrent-get"
  toJSON TorrentReannounce  = "torrent-reannounce"
  toJSON TorrentRemove      = "torrent-remove"
  toJSON TorrentSet         = "torrent-set"
  toJSON TorrentSetLocation = "torrent-set-location"
  toJSON TorrentStart       = "torrent-start"
  toJSON TorrentStartNow    = "torrent-start-now"
  toJSON TorrentStop        = "torrent-stop"
  toJSON TorrentVerify      = "torrent-verify"
  toJSON SessionGet         = "session-get"
  toJSON SessionStats       = "session-stats"
  toJSON PortTest           = "port-test"
  toJSON BlocklistUpdate    = "blocklist-update"
  toJSON FreeSpace          = "free-space"
  toJSON TorrentRenamePath  = "torrent-rename-path"

instance ToJSON ID where
  toJSON (ID i)   = toJSON i
  toJSON (Hash s) = toJSON s

instance FromJSON ID

instance ToJSON IDs where
  toJSON (IDs ids)      = toJSON ids
  toJSON RecentlyActive = "recently-active"

instance FromJSON IDs

instance ToJSON Error where
  toJSON OK      = Number 0
  toJSON Warning = Number 1
  toJSON Tracker = Number 2
  toJSON Local   = Number 3

instance FromJSON Error where
  parseJSON = withScientific "Error" $ \case
    0 -> pure OK
    1 -> pure Warning
    2 -> pure Tracker
    3 -> pure Local
    n -> error (show n ++ " is not a valid error code.")

instance ToJSON ETA where
  toJSON (ETA v) = toJSON v
  toJSON NA      = Number (-1)
  toJSON Unknown = Number (-2)

instance FromJSON ETA where
  parseJSON = withScientific "ETA" $ \case
                                          (-1) -> pure NA
                                          (-2) -> pure Unknown
                                          v -> if v >= 0 then pure (ETA . fromRational . toRational $ v) else prependFailure "Parsing ETA failed: " (fail ("Not an ETA " ++ show v))

instance ToJSON Status where
  toJSON Stopped         = Number 0
  toJSON CheckPending    = Number 1
  toJSON Checking        = Number 2
  toJSON DownloadPending = Number 3
  toJSON Downloading     = Number 4
  toJSON SeedPending     = Number 5
  toJSON Seeding         = Number 6

instance FromJSON Status where
  parseJSON = withScientific "Status" $ \case
                                              0 -> pure Stopped
                                              1 -> pure CheckPending
                                              2 -> pure Checking
                                              3 -> pure DownloadPending
                                              4 -> pure Downloading
                                              5 -> pure SeedPending
                                              6 -> pure Seeding
                                              v -> prependFailure "Parsing Status failed: " (fail ("not a Status value " ++ show v))

