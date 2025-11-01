{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Transmission.RPC.Torrent
  (
  -- * Torrent
  Torrent
  , availability
  , bandwidthPriority
  , corruptEver
  , hashString
  , desiredAvailable
  , downloadDir
  , downloadedEver
  , editDate
  , errorCode
  , errorString
  , eta
  , fileStats
  , files
  , haveUnchecked
  , haveValid
  , honorsSessionLimits
  , isFinished
  , isPrivate
  , labels
  , leftUntilDone
  , metadataPercentComplete
  , name
  , peerLimit
  , peersConnected
  , peers
  , peersFrom
  , peersGettingFromUs
  , peersSendingToUs
  , percentComplete
  , pieces
  , queuePosition
  , rateDownload
  , rateUpload
  , trackers
  , trackerList
  , trackerStats
  , toId
  , torrentFile
  , totalSize
  , webseedsSendingToUs
  , status
  , progress
  , ratio
  , activityDate
  , addedDate
  , startDate
  , doneDate
  , seedIdleMode
  , seedRatioLimit
  , seedRatioMode
  , uploadedEver
  , webseeds
  -- * ETA
  , ETA(..)
  -- Files
  , File
  , fName
  -- * File Statistics
  , FileStat
  , bytesCompleted
  -- * Tracker
  , Tracker
  -- * Status
  , Status (..)
  -- Tracker stats
  , TrackerStats
  , tsid
  , announceState
  , downloadCount
  , hasAnnounced
  , hasScraped
  , host
  , isBackup
  , lastAnnouncePeerCount
  , lastAnnounceResult
  , lastAnnounceStartTime
  , lastAnnounceSucceeded
  , lastAnnounceTime
  , lastAnnounceTimedOut
  , lastScrapeResult
  , lastScrapeStartTime
  , lastScrapeSucceeded
  , lastScrapeTime
  , lastScrapeTimedOut
  , leecherCount
  , nextAnnounceTime
  , nextScrapeTime
  , scrapeState
  , seederCount
  , siteName
  -- * shared functions
  , priority
  , mkTorrent
  )

where

import           Data.Aeson                 (FromJSON, ToJSON (toJSON),
                                             object, withObject, (.:), (.=), (.:?), Result (..), fromJSON, Value)
import           Data.Aeson.Types           (FromJSON (parseJSON))
import           Data.Maybe                 (catMaybes)
import           Data.Time                  (UTCTime)
import           GHC.Generics               (Generic)
import           Transmission.RPC.Constants (Priority)
import           Transmission.RPC.Enum      (IdleMode, RatioLimitMode)
import Data.Text (Text)
import Data.Ratio ((%))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import qualified Transmission.RPC.Types as T (Error(..))
import Transmission.RPC.Types (ETA(..), Status(..))

data File where
  File :: {fBytesCompleted :: Int,
           fLength :: Int,
           fName :: FilePath,
           beginPiece :: Maybe Int,
           endPiece :: Maybe Int} -> File deriving Show

data FileStat where
  FileStat :: {bytesCompleted :: Int,
                 tWanted :: Bool,
                 priority :: Priority} ->
                FileStat deriving Show

data Peer where
  Peer :: {address :: Text,
            clientName :: Text,
            clientIsChoked :: Bool,
            clientIsInterested :: Bool,
            flagStr :: Text,
            isDownloadingFrom :: Bool,
            isEncrypted :: Bool,
            isIncoming :: Bool,
            isUploadingTo :: Bool,
            isUTP :: Bool,
            peerIsChoked :: Bool,
            peerIsInterested :: Bool,
            port :: Int,
            pProgress :: Rational,
            rateToClient :: Int,
            rateToPeer :: Int
           } -> Peer deriving Show

data PeersFrom where
  PeersFrom :: {fromCache :: Int,
                fromDht :: Int,
                fromIncoming :: Int,
                fromLpd :: Int,
                fromLtep :: Int,
                fromPex :: Int,
                fromTracker :: Int} -> PeersFrom deriving (Show, Generic, FromJSON, ToJSON)

data Tracker where
  Tracker :: {tid :: Int,
                tAnnounce :: Text,
                tScrape :: Text,
                tTier :: Int} ->
               Tracker deriving Show

data TrackerStats where
  TrackerStats :: {tsid :: Int,
                     announceState :: Int,
                     tsAnnounce :: Text,
                     downloadCount :: Int,
                     hasAnnounced :: Bool,
                     hasScraped :: Bool,
                     host :: Text,
                     isBackup :: Bool,
                     lastAnnouncePeerCount :: Int,
                     lastAnnounceResult :: Text,
                     lastAnnounceStartTime :: UTCTime,
                     lastAnnounceSucceeded :: Bool,
                     lastAnnounceTime :: UTCTime,
                     lastAnnounceTimedOut :: Bool,
                     lastScrapeResult :: Text,
                     lastScrapeStartTime :: UTCTime,
                     lastScrapeSucceeded :: Bool,
                     lastScrapeTime :: UTCTime,
                     lastScrapeTimedOut :: Bool,
                     leecherCount :: Int,
                     nextAnnounceTime :: UTCTime,
                     nextScrapeTime :: UTCTime,
                     scrapeState :: Int,
                     tsScrape :: Text,
                     seederCount :: Int,
                     siteName :: Text,
                     tsTier :: Int} ->
                    TrackerStats deriving Show

data Torrent where
  Torrent :: {
                activityDate :: Maybe UTCTime,
                addedDate :: Maybe UTCTime,
                availability :: Maybe [Int],
                bandwidthPriority :: Maybe Priority,
                comment :: Maybe Text,
                corruptEver :: Maybe Int,
                creator :: Maybe Text,
                dateCreated :: Maybe UTCTime,
                desiredAvailable :: Maybe Int,
                doneDate :: Maybe UTCTime,
                downloadDir :: Maybe FilePath,
                downloadedEver :: Maybe Int,
                downloadLimit :: Maybe Int,
                downloadLimited :: Maybe Bool,
                editDate :: Maybe UTCTime,
                errorCode :: Maybe T.Error,
                errorString :: Maybe Text,
                eta :: Maybe ETA,
                etaIdle :: Maybe Int,
                fileCount :: Maybe Int,
                files :: Maybe [File],
                fileStats :: Maybe [FileStat],
                group :: Maybe Text,
                hashString :: Maybe Text,
                haveUnchecked :: Maybe Int,
                haveValid :: Maybe Int,
                honorsSessionLimits :: Maybe Bool,
                toId :: Maybe Int,
                isFinished :: Maybe Bool,
                isPrivate :: Maybe Bool,
                isStalled :: Maybe Bool,
                labels :: Maybe [Text],
                leftUntilDone :: Maybe Int,
                magnetLink :: Maybe Text,
                manualAnnounceTime :: Maybe UTCTime,
                maxConnectedPeers :: Maybe Int,
                metadataPercentComplete :: Maybe Rational,
                name :: Maybe Text,
                peerLimit :: Maybe Int,
                peers :: Maybe [Peer],
                peersConnected :: Maybe Int,
                peersFrom :: Maybe PeersFrom,
                peersGettingFromUs :: Maybe Int,
                peersSendingToUs :: Maybe Int,
                percentComplete :: Maybe Rational,
                progress :: Maybe Rational,
                pieces :: Maybe Text,
                pieceCount :: Maybe Int,
                pieceSize :: Maybe Int,
                priorities :: Maybe [Priority],
                primaryMimeType :: Maybe Text,
                queuePosition :: Maybe Int,
                rateDownload :: Maybe Int,
                rateUpload :: Maybe Int,
                recheckProgress :: Maybe Rational,
                secondsDownloading :: Maybe Int,
                secondsSeeding :: Maybe Int,
                seedIdleLimit :: Maybe Rational,
                seedIdleMode :: Maybe IdleMode,
                seedRatioLimit :: Maybe Rational,
                seedRatioMode :: Maybe RatioLimitMode,
                sequentialDownload :: Maybe Bool,
                sizeWhenDone :: Maybe Int,
                startDate :: Maybe UTCTime,
                status :: Maybe Status,
                trackers :: Maybe [Tracker],
                trackerList :: Maybe Text,
                trackerStats :: Maybe [TrackerStats],
                totalSize :: Maybe Int,
                torrentFile :: Maybe FilePath,
                uploadedEver :: Maybe Int,
                uploadLimit :: Maybe Int,
                uploadLimited :: Maybe Bool,
                ratio :: Maybe Rational,
                toWanted :: Maybe [Int],
                webseeds :: Maybe [Text],
                webseedsSendingToUs :: Maybe Int} ->
               Torrent deriving (Show, Generic)

instance ToJSON File where
  toJSON (File b l n bp ep) = object . catMaybes $ [Just ("bytesCompleted" .= b), Just ("length" .= l), Just ("name" .= n), 
    ("begin_piece" .=) <$> bp, ("end_piece" .=) <$> ep]

instance FromJSON File where
  parseJSON = withObject "File" $ \v -> File
    <$> v .: "bytesCompleted"
    <*> v .: "length"
    <*> v .: "name"
    <*> v .:? "begin_piece"
    <*> v .:? "end_piece"

instance ToJSON FileStat where
  toJSON (FileStat bc w p) = object ["bytesCompleted" .= bc, "wanted" .= w, "priority" .= p]

instance FromJSON FileStat where
  parseJSON = withObject "FileStat" $ \v -> FileStat
    <$> v .: "bytesCompleted"
    <*> v .: "wanted"
    <*> v .: "priority"

instance ToJSON Peer where
  toJSON p = object [
    "address" .= address p,
    "clientName" .= clientName p,
    "clientIsChoked" .= clientIsChoked p,
    "clientIsInterested" .= clientIsInterested p,
    "flagStr" .= flagStr p,
    "isDownloadingFrom" .= isDownloadingFrom p,
    "isEncrypted" .= isEncrypted p,
    "isIncoming" .= isIncoming p,
    "isUploadingTo" .= isUploadingTo p,
    "isUTP" .= isUTP p,
    "peerIsChoked" .= peerIsChoked p,
    "peerIsInterested" .= peerIsInterested p,
    "port" .= port p,
    "progress" .= pProgress p,
    "rateToClient" .= rateToClient p,
    "rateToPeer" .= rateToPeer p
                    ]

instance FromJSON Peer where
  parseJSON = withObject "Peer" $ \v -> Peer
    <$> v .: "address"
    <*> v .: "clientName"
    <*> v .: "clientIsChoked"
    <*> v .: "clientIsInterested"
    <*> v .: "flagStr"
    <*> v .: "isDownloadingFrom"
    <*> v .: "isEncrypted"
    <*> v .: "isIncoming"
    <*> v .: "isUploadingTo"
    <*> v .: "isUTP"
    <*> v .: "peerIsChoked"
    <*> v .: "peerIsInterested"
    <*> v .: "port"
    <*> v .: "progress"
    <*> v .: "rateToClient"
    <*> v .: "rateToPeer"

instance ToJSON Tracker where
  toJSON (Tracker ti a s t) = object ["id" .= ti, "announce" .= a, "scrape" .= s, "tier" .= t]

instance FromJSON Tracker where
  parseJSON = withObject "Tracker" $ \v -> Tracker
    <$> v .: "id"
    <*> v .: "announce"
    <*> v .: "scrape"
    <*> v .: "tier"

instance ToJSON TrackerStats where
  toJSON (TrackerStats i as a dc ha hs h ib lapc lar lasti las lat lato lsr lsst lss lst lsto lc nat nst ss s sc sn t) = object [
    "id" .= i,
    "announceState" .= as,
    "announce" .= a,
    "downloadCount" .= dc,
    "hasAnnounced" .= ha,
    "hasScraped" .= hs,
    "host" .= h,
    "isBackup" .= ib,
    "lastAnnouncePeerCount" .= lapc,
    "lastAnnounceResult" .= lar,
    ("lastAnnounceStartTime" .=) . utcTimeToPOSIXSeconds $ lasti,
    "lastAnnounceSucceeded" .= las,
    ("lastAnnounceTime" .=) . utcTimeToPOSIXSeconds $ lat,
    "lastAnnounceTimedOut" .= lato,
    "lastScrapeResult" .= lsr,
    ("lastScrapeStartTime" .=) . utcTimeToPOSIXSeconds $ lsst,
    "lastScrapeSucceeded" .= lss,
    ("lastScrapeTime" .=) . utcTimeToPOSIXSeconds $ lst,
    "lastScrapeTimedOut" .= lsto,
    "leecherCount" .= lc,
    ("nextAnnounceTime" .=) . utcTimeToPOSIXSeconds $ nat,
    ("nextScrapeTime" .=) . utcTimeToPOSIXSeconds $ nst,
    "scrape" .= s,
    "scrapeState" .= ss,
    "seederCount" .= sc,
    "sitename" .= sn,
    "tier" .= t
    ]

instance FromJSON TrackerStats where
  parseJSON = withObject "TrackerStats" $ \v -> TrackerStats
    <$> v .: "id"
    <*> v .: "announceState"
    <*> v .: "announce"
    <*> v .: "downloadCount"
    <*> v .: "hasAnnounced"
    <*> v .: "hasScraped"
    <*> v .: "host"
    <*> v .: "isBackup"
    <*> v .: "lastAnnouncePeerCount"
    <*> v .: "lastAnnounceResult"
    <*> (posixSecondsToUTCTime <$> v .: "lastAnnounceStartTime")
    <*> v .: "lastAnnounceSucceeded"
    <*> (posixSecondsToUTCTime <$> v .: "lastAnnounceTime")
    <*> v .: "lastAnnounceTimedOut"
    <*> v .: "lastScrapeResult"
    <*> (posixSecondsToUTCTime <$> v .: "lastScrapeStartTime")
    <*> v .: "lastScrapeSucceeded"
    <*> (posixSecondsToUTCTime <$> v .: "lastScrapeTime")
    <*> v .: "lastScrapeTimedOut"
    <*> v .: "leecherCount"
    <*> (posixSecondsToUTCTime <$> v .: "nextAnnounceTime")
    <*> (posixSecondsToUTCTime <$> v .: "nextScrapeTime")
    <*> v .: "scrapeState"
    <*> v .: "scrape"
    <*> v .: "seederCount"
    <*> v .: "sitename"
    <*> v .: "tier"

instance ToJSON Torrent where
  toJSON t = object . catMaybes $ [
                    ("activityDate" .=) . utcTimeToPOSIXSeconds <$> activityDate t,
                    ("addedDate" .=) . utcTimeToPOSIXSeconds <$> addedDate t,
                    ("availability" .=) <$> availability t,
                    ("bandwidthPriority" .=) <$> bandwidthPriority t,
                    ("comment" .=) <$> comment t,
                    ("corruptEver" .=) <$> corruptEver t,
                    ("creator" .=) <$> creator t,
                    ("dateCreated" .=) . utcTimeToPOSIXSeconds <$> dateCreated t,
                    ("desiredAvailable" .=) <$> desiredAvailable t,
                    ("doneDate" .=) . utcTimeToPOSIXSeconds <$> doneDate t,
                    ("downloadDir" .=) <$> downloadDir t,
                    ("downloadedEver" .=) <$> downloadedEver t,
                    ("downloadLimit" .=) <$> downloadLimit t,
                    ("downloadLimited" .=) <$> downloadLimited t,
                    ("editDate" .=) . utcTimeToPOSIXSeconds <$> editDate t,
                    ("error" .=) <$> errorCode t,
                    ("errorString" .=) <$> errorString t,
                    ("eta" .=) <$> eta t,
                    ("etaIdle" .=) <$> etaIdle t,
                    ("file-count" .=) <$> fileCount t,
                    ("files" .=) <$> files t,
                    ("fileStats" .=) <$> fileStats t,
                    ("group" .=) <$> group t,
                    ("hashSTring" .=) <$> hashString t,
                    ("haveUnchecked" .=) <$> haveUnchecked t,
                    ("haveValid" .=) <$> haveValid t,
                    ("honorsSessionLimits" .=) <$> honorsSessionLimits t,
                    ("id" .=) <$> toId t,
                    ("isFinished" .=) <$> isFinished t,
                    ("isPrivate" .=) <$> isPrivate t,
                    ("isStalled" .=) <$> isStalled t,
                    ("labels" .=) <$> labels t,
                    ("leftUntilDone" .=) <$> leftUntilDone t,
                    ("magnetLink" .=) <$> magnetLink t,
                    ("manualAnnounceTime" .=) . utcTimeToPOSIXSeconds <$> manualAnnounceTime t,
                    ("maxConnectedPeers" .=) <$> maxConnectedPeers t,
                    ("metadataPercentComplete" .=) <$> metadataPercentComplete t,
                    ("name" .=) <$> name t,
                    ("peers" .=) <$> peers t,
                    ("peersConnected" .=) <$> peersConnected t,
                    ("peersFrom" .=) <$> peersFrom t,
                    ("peer-limit" .=) <$> peerLimit t,
                    ("peersGettingFromUs" .=) <$> peersGettingFromUs t,
                    ("peersSendingToUs" .=) <$> peersSendingToUs t,
                    ("percentComplete" .=) . (* (1 % 100)) <$> percentComplete t,
                    ("percentDone" .=) . (* (1 % 100)) <$> progress t,
                    ("pieces" .=) <$> pieces t,
                    ("pieceCount" .=) <$> pieceCount t,
                    ("pieceSize" .=) <$> pieceSize t,
                    ("priorities" .=) <$> priorities t,
                    ("primary-mime-type" .=) <$> primaryMimeType t,
                    ("queuePosition" .=) <$> queuePosition t,
                    ("rateDownload" .=) <$> rateDownload t,
                    ("rateUpload" .=) <$> rateUpload t,
                    ("recheckProgress" .=) <$> recheckProgress t,
                    ("secondsDownloading" .=) <$> secondsDownloading t,
                    ("secondsSeeding" .=) <$> secondsSeeding t,
                    ("seedIdleLimit" .=) <$> seedIdleLimit t,
                    ("seedIdleMode" .=) <$> seedIdleMode t,
                    ("seedRatioLimit" .=) <$> seedRatioLimit t,
                    ("seedRatioMode" .=) <$> seedRatioMode t,
                    ("sequential_download" .=) <$> sequentialDownload t,
                    ("sizeWhenDone" .=) <$> sizeWhenDone t,
                    ("startDate" .=) . utcTimeToPOSIXSeconds <$> startDate t,
                    ("status" .=) <$> status t,
                    ("trackers" .=) <$> trackers t,
                    ("trackerList" .=) <$> trackerList t,
                    ("trackerStats" .=) <$> trackerStats t,
                    ("totalSize" .=) <$> totalSize t,
                    ("torrentFile" .=) <$> torrentFile t,
                    ("uploadedEver" .=) <$> uploadedEver t,
                    ("uploadLimit" .=) <$> uploadLimit t,
                    ("uploadLimited" .=) <$> uploadLimited t,
                    ("uploadRatio" .=) <$> ratio t,
                    ("wanted" .=) <$> toWanted t,
                    ("webseeds" .=) <$> webseeds t,
                    ("webseedsSendingToUs" .=) <$> webseedsSendingToUs t
                    ]

instance FromJSON Torrent where
  parseJSON = withObject "Torrent" $ \v -> (Torrent .
    (posixSecondsToUTCTime <$>) <$> (v .:? "activityDate"))
    <*> ((posixSecondsToUTCTime <$>) <$> (v .:? "addedDate"))
    <*> v .:? "availability"
    <*> v .:? "bandwidthPriority"
    <*> v .:? "comment"
    <*> v .:? "corruptEver"
    <*> v .:? "creator"
    <*> ((posixSecondsToUTCTime <$>) <$> (v .:? "dateCreated"))
    <*> v .:? "desiredAvailable"
    <*> ((posixSecondsToUTCTime <$>) <$> (v .:? "doneDate"))
    <*> v .:? "downloadDir"
    <*> v .:? "downloadedEver"
    <*> v .:? "downloadLimit"
    <*> v .:? "downloadLimited"
    <*> ((posixSecondsToUTCTime <$>) <$> (v .:? "editDate"))
    <*> v .:? "error"
    <*> v .:? "errorString"
    <*> v .:? "eta"
    <*> v .:? "etaIdle"
    <*> v .:? "file-count"
    <*> v .:? "files"
    <*> v .:? "fileStats"
    <*> v .:? "group"
    <*> v .:? "hashString"
    <*> v .:? "haveUnchecked"
    <*> v .:? "haveValid"
    <*> v .:? "honorsSessionLimits"
    <*> v .:? "id"
    <*> v .:? "isFinished"
    <*> v .:? "isPrivate"
    <*> v .:? "isStalled"
    <*> v .:? "labels"
    <*> v .:? "leftUntilDone"
    <*> v .:? "magnetLink"
    <*> ((posixSecondsToUTCTime <$>) <$> (v .:? "manualAnnounceTime"))
    <*> v .:? "maxConnectedPeers"
    <*> (((100 *) <$>) <$> (v .:? "metadataPercentComplete"))
    <*> v .:? "name"
    <*> v .:? "peer-limit"
    <*> v .:? "peers"
    <*> v .:? "peersConnected"
    <*> v .:? "peersFrom"
    <*> v .:? "peersGettingFromUs"
    <*> v .:? "peersSendingToUs"
    <*> (((100 *) <$>) <$> (v .:? "percentComplete"))
    <*> (((100 *) <$>) <$> (v .:? "percentDone"))
    <*> v .:? "pieces"
    <*> v .:? "pieceCount"
    <*> v .:? "pieceSize"
    <*> v .:? "priorities"
    <*> v .:? "primary-mime-type"
    <*> v .:? "queuePosition"
    <*> v .:? "rateDownload"
    <*> v .:? "rateUpload"
    <*> v .:? "recheckProgress"
    <*> v .:? "secondsDownloading"
    <*> v .:? "secondsSeeding"
    <*> v .:? "seedIdleLimit"
    <*> v .:? "seedIdleMode"
    <*> v .:? "seedRatioLimit"
    <*> v .:? "seedRatioMode"
    <*> v .:? "sequential_download"
    <*> v .:? "sizeWhenDone"
    <*> ((posixSecondsToUTCTime <$>) <$> (v .:? "startDate"))
    <*> v .:? "status"
    <*> v .:? "trackers"
    <*> v .:? "trackerList"
    <*> v .:? "trackerStats"
    <*> v .:? "totalSize"
    <*> v .:? "torrentFile"
    <*> v .:? "uploadedEver"
    <*> v .:? "uploadLimit"
    <*> v .:? "uploadLimited"
    <*> v .:? "uploadRatio"
    <*> v .:? "wanted"
    <*> v .:? "webseeds"
    <*> v .:? "webseedsSendingToUs"

mkTorrent :: Value -> Torrent
mkTorrent v = case fromJSON v of
                Error s -> error s
                Success a -> a
