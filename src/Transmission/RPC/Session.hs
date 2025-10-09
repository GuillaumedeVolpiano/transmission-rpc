{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Transmission.RPC.Session
  (
  Session,
  altSpeedDown,
  altSpeedEnabled,
  altSpeedTimeBegin,
  altSpeedTimeDay,
  altSpeedTimeEnabled,
  altSpeedTimeEnd,
  altSpeedUp,
  blockListEnabled,
  blockListSize,
  blockListURL,
  cacheSizeMB,
  configDir,
  defaultTrackers,
  dhtEnabled,
  downloadDir,
  downloadQueueEnabled,
  downloadQueueSize,
  encryption,
  idleSeedingLimitEnabled,
  idleSeedingLimit,
  incompleteDirEnabled,
  incompleteDir,
  lpdEnabled,
  peerLimitGlobal,
  peerLimitPerTorrent,
  peerPortRandomOnStart,
  peerPort,
  pexEnabled,
  portForwardingEnabled,
  queueStalledEnabled,
  queueStalledMinutes,
  renamePartialFiles,
  reqq,
  rpcVersionMinimum,
  rpcVersionSemver,
  rpcVersion,
  scriptTorrentAddedEnabled,
  scriptTorrentAddedFilename,
  scriptTorrentDoneEnabled,
  scriptTorrentDoneFilename,
  scriptTorrentDoneSeedingEnabled,
  scriptTorrentDoneSeedingFilename,
  seedQueueEnabled,
  seedQueueSize,
  seedRatioLimit,
  seedRatioLimited,
  sequentialDownload,
  sessionId,
  speedLimitDownEnabled,
  speedLimitDown,
  speedLimitUpEnabled,
  speedLimitUp,
  startAddedTorrents,
  trashOriginalTorrentFile,
  Units,
  units,
  utpEnabled,
  version,
  speedUnits,
  speedBytes,
  sizeUnits,
  sizeBytes,
  memoryUnits,
  memoryBytes,
   )
where
import           Data.Aeson            (FromJSON, ToJSON, object, parseJSON,
                                        withObject, (.:), (.:?), (.=))
import           Data.Aeson.Types      (toJSON)
import           Data.Maybe            (catMaybes)
import           Data.Text             (Text)
import           Transmission.RPC.Enum (EncryptionMode)

data Session where
  Session :: {
                altSpeedDown :: Maybe Int,
                altSpeedEnabled :: Maybe Bool,
                altSpeedTimeBegin :: Maybe Int,
                altSpeedTimeDay :: Maybe Int,
                altSpeedTimeEnabled :: Maybe Bool,
                altSpeedTimeEnd :: Maybe Int,
                altSpeedUp :: Maybe Int,
                blockListEnabled :: Maybe Bool,
                blockListSize :: Maybe Int,
                blockListURL :: Maybe Text,
                cacheSizeMB :: Maybe Int,
                configDir :: Maybe FilePath,
                defaultTrackers :: Maybe Text,
                dhtEnabled :: Maybe Bool,
                downloadDir :: Maybe FilePath,
                downloadQueueEnabled :: Maybe Bool,
                downloadQueueSize :: Maybe Int,
                encryption :: Maybe EncryptionMode,
                idleSeedingLimitEnabled :: Maybe Bool,
                idleSeedingLimit :: Maybe Int,
                incompleteDirEnabled :: Maybe Bool,
                incompleteDir :: Maybe FilePath,
                lpdEnabled :: Maybe Bool,
                peerLimitGlobal :: Maybe Int,
                peerLimitPerTorrent :: Maybe Int,
                peerPortRandomOnStart :: Maybe Bool,
                peerPort :: Maybe Int,
                pexEnabled :: Maybe Bool,
                portForwardingEnabled :: Maybe Bool,
                queueStalledEnabled :: Maybe Bool,
                queueStalledMinutes :: Maybe Int,
                renamePartialFiles :: Maybe Bool,
                reqq :: Maybe Int,
                rpcVersionMinimum :: Maybe Int,
                rpcVersionSemver :: Maybe Text,
                rpcVersion :: Maybe Int,
                scriptTorrentAddedEnabled :: Maybe Bool,
                scriptTorrentAddedFilename :: Maybe FilePath,
                scriptTorrentDoneEnabled :: Maybe Bool,
                scriptTorrentDoneFilename :: Maybe FilePath,
                scriptTorrentDoneSeedingEnabled :: Maybe Bool,
                scriptTorrentDoneSeedingFilename :: Maybe FilePath,
                seedQueueEnabled :: Maybe Bool,
                seedQueueSize :: Maybe Int,
                seedRatioLimit :: Maybe Rational,
                seedRatioLimited :: Maybe Bool,
                sequentialDownload :: Maybe Bool,
                sessionId :: Maybe Text,
                speedLimitDownEnabled :: Maybe Bool,
                speedLimitDown :: Maybe Int,
                speedLimitUpEnabled :: Maybe Bool,
                speedLimitUp :: Maybe Int,
                startAddedTorrents :: Maybe Bool,
                trashOriginalTorrentFile :: Maybe Bool,
                units :: Maybe Units,
                utpEnabled :: Maybe Bool,
                version :: Maybe Text
                } -> Session deriving Show

data Units where
  Units :: {
            speedUnits :: [Text],
            speedBytes :: Int,
            sizeUnits :: [Text],
            sizeBytes :: Int,
            memoryUnits :: [Text],
            memoryBytes :: Int
           } -> Units deriving Show

instance ToJSON Units where
  toJSON (Units spu spb siu sib mu mb) = object ["speed-units" .= spu, "speed-bytes" .= spb, "size-units" .= siu, "size-bytes" .= sib, "memory-units" .= mu, "memory-bytes" .= mb]

instance FromJSON Units where
  parseJSON = withObject "Units" $ \v -> Units
    <$> v .: "speed-units"
    <*> v .: "speed-bytes"
    <*> v .: "size-units"
    <*> v .: "size-bytes"
    <*> v .: "memory-units"
    <*> v .: "memory-bytes"

instance ToJSON Session where
  toJSON s = object .catMaybes $ [
    ("alt-speed-down" .=) <$> altSpeedDown s,
    ("alt-speed-enabled" .=) <$> altSpeedEnabled s,
    ("alt-speed-time-begin" .=) <$> altSpeedTimeBegin s,
    ("alt-speed-time-day" .=) <$> altSpeedTimeDay s,
    ("alt-speed-time-enabled" .=) <$> altSpeedTimeEnabled s,
    ("alt-speed-time-end" .=) <$> altSpeedTimeEnd s,
    ("alt-speed-up" .=) <$> altSpeedUp s,
    ("block-list-enabled" .=) <$> blockListEnabled s,
    ("block-list-size" .=) <$> blockListSize s,
    ("block-list-url" .=) <$> blockListURL s,
    ("cache-size-mb" .=) <$> cacheSizeMB s,
    ("config-dir" .=) <$> configDir s,
    ("default-trackers"  .=) <$> defaultTrackers s,
    ("dht-enabled" .=) <$> dhtEnabled s,
    ("download-dir" .=) <$> downloadDir s,
    ("download-queue-enabled" .=) <$> downloadQueueEnabled s,
    ("download-queue-size" .=) <$> downloadQueueSize s,
    ("encryption" .=) <$> encryption s,
    ("idle-seeeding-limit-enabled" .=) <$> idleSeedingLimitEnabled s,
    ("idle-seeding-limit" .=) <$> idleSeedingLimit s,
    ("incomplete-dir-enabled" .=) <$> incompleteDirEnabled s,
    ("incomplete-dir" .=) <$> incompleteDir s,
    ("lpd-enabled" .=) <$> lpdEnabled s,
    ("peer-limit-global" .=) <$> peerLimitGlobal s,
    ("peer-limit-per-torrent" .=) <$> peerLimitPerTorrent s,
    ("peer-port-random-on-start" .=) <$> peerPortRandomOnStart s,
    ("peer-port" .=) <$> peerPort s,
    ("pex-enabled" .=) <$> pexEnabled s,
    ("port-forwarding-enabled" .=) <$> portForwardingEnabled s,
    ("queue-stalled-enabled" .=) <$> queueStalledEnabled s,
    ("queue-stalled-minutes" .=) <$> queueStalledMinutes s,
    ("rename-partial-files" .=) <$> renamePartialFiles s,
    ("reqq" .=) <$> reqq s,
    ("rpc-version-minimun" .=) <$> rpcVersionMinimum s,
    ("rpc-version-semver" .=) <$> rpcVersionSemver s,
    ("rpc-version" .=) <$> rpcVersion s,
    ("script-torrent-added-enabled" .=) <$> scriptTorrentAddedEnabled s,
    ("script-torrent-added-filename" .=) <$> scriptTorrentAddedFilename s,
    ("script-torrent-done-enabled" .=) <$> scriptTorrentDoneEnabled s,
    ("script-torrent-done-filename" .=) <$> scriptTorrentDoneFilename s,
    ("script-torrent-done-seeding-enabled" .=) <$> scriptTorrentDoneSeedingEnabled s,
    ("script-torrent-done-seeding-filename" .=) <$> scriptTorrentDoneSeedingFilename s,
    ("seed-queue-enabled" .=) <$> seedQueueEnabled s,
    ("seed-queue-size" .=) <$> seedQueueSize s,
    ("seedRatioLimit" .=) <$> seedRatioLimit s,
    ("seedRatioLimited" .=) <$> seedRatioLimited s,
    ("sequential_download" .=) <$> sequentialDownload s,
    ("session-id" .=) <$> sessionId s,
    ("speed-limit-down-enabled" .=) <$> speedLimitDownEnabled s,
    ("speed-limit-down" .=) <$> speedLimitDown s,
    ("speed-limit-up-enabled" .=) <$> speedLimitUpEnabled s,
    ("speed-limit-up" .=) <$> speedLimitUp s,
    ("start-added-torrents" .=) <$> startAddedTorrents s,
    ("trash-original-torrent-files" .=) <$> trashOriginalTorrentFile s,
    ("units" .=) <$> units s,
    ("utp-enabled" .=) <$> utpEnabled s,
    ("version" .=) <$> version s
                    ]

instance FromJSON Session where
  parseJSON = withObject "Session" $ \v -> Session
    <$> v .:? "alt-speed-down"
    <*> v .:? "alt-speed-enabled"
    <*> v .:? "alt-speed-time-begin"
    <*> v .:? "alt-speed-time-day"
    <*> v .:? "alt-speed-time-enabled"
    <*> v .:? "alt-speed-time-end"
    <*> v .:? "alt-speed-up"
    <*> v .:? "block-list-enabled"
    <*> v .:? "block-list-size"
    <*> v .:? "block-list-url"
    <*> v .:? "cache-size-mb"
    <*> v .:? "config-dir"
    <*> v .:? "default-trackers"
    <*> v .:? "dht-enabled"
    <*> v .:? "download-dir"
    <*> v .:? "download-queue-enabled"
    <*> v .:? "download-queue-size"
    <*> v .:? "encryption"
    <*> v .:? "idle-seeeding-limit-enabled"
    <*> v .:? "idle-seeding-limit"
    <*> v .:? "incomplete-dir-enabled"
    <*> v .:? "incomplete-dir"
    <*> v .:? "lpd-enabled"
    <*> v .:? "peer-limit-global"
    <*> v .:? "peer-limit-per-torrent"
    <*> v .:? "peer-port-random-on-start"
    <*> v .:? "peer-port"
    <*> v .:? "pex-enabled"
    <*> v .:? "port-forwarding-enabled"
    <*> v .:? "queue-stalled-enabled"
    <*> v .:? "queue-stalled-minutes"
    <*> v .:? "rename-partial-files"
    <*> v .:? "reqq"
    <*> v .:? "rpc-version-minimun"
    <*> v .:? "rpc-version-semver"
    <*> v .:? "rpc-version"
    <*> v .:? "script-torrent-added-enabled"
    <*> v .:? "script-torrent-added-filename"
    <*> v .:? "script-torrent-done-enabled"
    <*> v .:? "script-torrent-done-filename"
    <*> v .:? "script-torrent-done-seeding-enabled"
    <*> v .:? "script-torrent-done-seeding-filename"
    <*> v .:? "seed-queue-enabled"
    <*> v .:? "seed-queue-size"
    <*> v .:? "seedRatioLimit"
    <*> v .:? "seedRatioLimited"
    <*> v .:? "sequential_download"
    <*> v .:? "session-id"
    <*> v .:? "speed-limit-down-enabled"
    <*> v .:? "speed-limit-down"
    <*> v .:? "speed-limit-up-enabled"
    <*> v .:? "speed-limit-up"
    <*> v .:? "start-added-torrents"
    <*> v .:? "trash-original-torrent-files"
    <*> v .:? "units"
    <*> v .:? "utp-enabled"
    <*> v .:? "version"
