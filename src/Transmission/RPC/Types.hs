{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
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
  , getHttpSession
  , getServerVersion
  , getSemVerVersion
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

import           Data.Aeson               (FromJSON, ToJSON, toJSON)
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import           Effectful                (Eff, (:>))
import           Effectful.FileSystem.IO  (Handle)
import           Effectful.Internal.Monad (Prim)
import           Effectful.Prim.IORef     (IORef, newIORef)
import           Effectful.Wreq           (Options)
import           Effectful.Wreq.Session   (Session)
import           GHC.Generics             (Generic)
import qualified Transmission.RPC.Session as TS (Session)
import           Transmission.RPC.Session (emptySession)

data Client where
  Client :: {getURI :: URI,
               getSession :: IORef TS.Session,
               getHttpSession :: Session,
               getOpts :: IORef Options,
               getProtocolVersion :: IORef Int,
               getServerVersion :: IORef (Maybe Text),
               getSemVerVersion :: IORef (Maybe Text)} ->
              Client

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

data RPCMethod = GroupSet | GroupGet | QueueMoveBottom | QueueMoveDown | QueueMoveTop | QueueMoveUp | TorrentAdd | TorrentGet | TorrentReannounce | TorrentRemove | TorrentSetLocation | TorrentSet | TorrentStart | TorrentStartNow | TorrentStop | TorrentVerify | SessionGet | SessionStats | PortTest | BlocklistUpdate | FreeSpace | TorrentRenamePath deriving Show

data JSONTypes = JSONNumber | JSONDouble | JSONObject | JSONString | JSONArray | JSONBool deriving Show

data Args = Args JSONTypes Int (Maybe Int) (Maybe String) (Maybe String) String deriving Show

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

newClient :: (Prim :> es) => URI -> Session -> Options -> Int -> Eff es Client
newClient uri session opts pv = do
  sesh <- newIORef emptySession
  opts' <- newIORef opts
  pv' <- newIORef pv
  srv <- newIORef Nothing
  smv <- newIORef Nothing
  pure $ Client uri sesh session opts' pv' srv smv
