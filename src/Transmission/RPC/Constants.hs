{-# LANGUAGE LambdaCase #-}
module Transmission.RPC.Constants
  (defaultTimeout
  , sessionIdHeaderName
  , torrentGetArgs
  , Priority)
where

import           Data.ByteString        (ByteString)
import           Data.ByteString.Char8  (pack)
import           Data.CaseInsensitive   (CI, mk)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as M (fromList)
import           Transmission.RPC.Types (Args (Args), JSONTypes (..))
import Data.Aeson (ToJSON (toJSON), Value (Number), FromJSON, withScientific)
import Data.Aeson.Types (FromJSON(parseJSON), prependFailure)

defaultTimeout :: Int
defaultTimeout = 3000

data Priority = Low | Normal | High deriving (Eq, Ord, Show)

instance ToJSON Priority where
  toJSON Low = Number (-1) 
  toJSON Normal = Number 0
  toJSON High = Number 1

instance FromJSON Priority where
  parseJSON = withScientific "Priority" $ \case
                                                           (-1) -> pure Low
                                                           0 -> pure Normal
                                                           1 -> pure High
                                                           x -> prependFailure "Parsing Priority failed: " (fail (show x ++ " is not a valid priority"))


sessionIdHeaderName :: CI ByteString
sessionIdHeaderName = mk . pack $ "X-Transmission-Session-Id"

torrentGetArgs:: HashMap String Args
torrentGetArgs = M.fromList [
    ("activityDate", Args JSONNumber 1 Nothing Nothing Nothing "Last time of upload or download activity."),
    ("addedDate", Args JSONNumber 1 Nothing Nothing Nothing "The date when this torrent was first added."),
    ("bandwidthPriority", Args JSONNumber 5 Nothing Nothing Nothing "Bandwidth priority. Low (-1), Normal (0) or High (1)."),
    ("comment", Args JSONString 1 Nothing Nothing Nothing "Torrent comment."),
    ("corruptEver", Args JSONNumber 1 Nothing Nothing Nothing "Number of bytes of corrupt data downloaded."),
    ("creator", Args JSONString 1 Nothing Nothing Nothing "Torrent creator."),
    ("dateCreated", Args JSONNumber 1 Nothing Nothing Nothing "Torrent creation date."),
    ("desiredAvailable", Args JSONNumber 1 Nothing Nothing Nothing "Number of bytes available and left to be downloaded."),
    ("doneDate", Args JSONNumber 1 Nothing Nothing Nothing "The date when the torrent finished downloading."),
    ("downloadDir", Args JSONString 4 Nothing Nothing Nothing "The directory path where the torrent is downloaded to."),
    ("downloadedEver", Args JSONNumber 1 Nothing Nothing Nothing "Number of bytes of good data downloaded."),
    ("downloadLimit", Args JSONNumber 1 Nothing Nothing Nothing "Download limit in Kbps."),
    ("downloadLimitMode", Args
        JSONNumber 1 (Just 5) Nothing Nothing "Download limit mode. 0 means global, 1 means single, 2 unlimited."
    ),
    ("downloadLimited", Args JSONBool 5 Nothing Nothing Nothing "Download limit is enabled"),
    ("editDate", Args JSONNumber 16 Nothing Nothing Nothing ""),
    ("error", Args
        JSONNumber
        1 Nothing Nothing Nothing
        "Kind of error. 0 means OK, 1 means tracker warning, 2 means tracker error, 3 means local error."
    ),
    ("errorString", Args JSONNumber 1 Nothing Nothing Nothing "Error message."),
    ("eta", Args
        JSONNumber
        1 Nothing Nothing Nothing
        "Estimated number of seconds left when downloading or seeding. -1 means not available and -2 means unknown."
    ),
    ("etaIdle", Args
        JSONNumber
        15 Nothing Nothing Nothing
        "Estimated number of seconds left until the idle time limit is reached. -1 means not available and -2 means unknown."
    ),
    ("files", Args JSONArray 1 Nothing Nothing Nothing "Array of file object containing key, bytesCompleted, length and name."),
    ("fileStats", Args
        JSONArray 5 Nothing Nothing Nothing "Away of file statistics containing bytesCompleted, wanted and priority."
    ),
    ("group", Args JSONString 17 Nothing Nothing Nothing "The name of this torrent's bandwidth group"),
    ("hashString", Args JSONString 1 Nothing Nothing Nothing "Hashstring unique for the torrent even between sessions."),
    ("haveUnchecked", Args JSONNumber 1 Nothing Nothing Nothing "Number of bytes of partial pieces."),
    ("haveValid", Args JSONNumber 1 Nothing Nothing Nothing "Number of bytes of checksum verified data."),
    ("honorsSessionLimits", Args JSONBool 5 Nothing Nothing Nothing "True if session upload limits are honored"),
    ("id", Args JSONNumber 1 Nothing Nothing Nothing "Session unique torrent id."),
    ("isFinished", Args JSONBool 9 Nothing Nothing Nothing "True if the torrent is finished. Downloaded and seeded."),
    ("isPrivate", Args JSONBool 1 Nothing Nothing Nothing "True if the torrent is private."),
    ("isStalled", Args JSONBool 14 Nothing Nothing Nothing "True if the torrent has stalled (been idle for a long time)."),
    ("labels", Args JSONArray 16 Nothing Nothing Nothing "array of string labels"),
    ("leftUntilDone", Args JSONNumber 1 Nothing Nothing Nothing "Number of bytes left until the download is done."),
    ("magnetLink", Args JSONString 7 Nothing Nothing Nothing "The magnet link for this torrent."),
    ("manualAnnounceTime", Args JSONNumber 1 Nothing Nothing Nothing "The time until you manually ask for more peers."),
    ("maxConnectedPeers", Args JSONNumber 1 Nothing Nothing Nothing "Maximum of connected peers."),
    ("metadataPercentComplete", Args JSONDouble 7 Nothing Nothing Nothing "Download progress of metadata. 0.0 to 1.0."),
    ("name", Args JSONString 1 Nothing Nothing Nothing "Torrent name."),
    ("peer-limit", Args JSONNumber 5 Nothing Nothing Nothing "Maximum number of peers."),
    ("peers", Args JSONArray 2 Nothing Nothing Nothing "Array of peer objects."),
    ("peersConnected", Args JSONNumber 1 Nothing Nothing Nothing "Number of peers we are connected to."),
    ("peersFrom", Args JSONObject 1 Nothing Nothing Nothing "Object containing download peers counts for different peer types."),
    ("peersGettingFromUs", Args JSONNumber 1 Nothing Nothing Nothing "Number of peers we are sending data to."),
    ("peersSendingToUs", Args JSONNumber 1 Nothing Nothing Nothing "Number of peers sending to us"),
    ("percentComplete", Args JSONDouble 17 Nothing Nothing Nothing ""),
    ("percentDone", Args JSONDouble 5 Nothing Nothing Nothing "Download progress of selected files. 0.0 to 1.0."),
    ("pieces", Args JSONString 5 Nothing Nothing Nothing "String with base64 encoded bitfield indicating finished pieces."),
    ("pieceCount", Args JSONNumber 1 Nothing Nothing Nothing "Number of pieces."),
    ("pieceSize", Args JSONNumber 1 Nothing Nothing Nothing "Number of bytes in a piece."),
    ("priorities", Args JSONArray 1 Nothing Nothing Nothing "Array of file priorities."),
    ("primary-mime-type", Args JSONString 17 Nothing Nothing Nothing ""),
    ("queuePosition", Args JSONNumber 14 Nothing Nothing Nothing "The queue position."),
    ("rateDownload", Args JSONNumber 1 Nothing Nothing Nothing "(B/s)"),
    ("rateUpload", Args JSONNumber 1 Nothing Nothing Nothing "(B/s)"),
    ("recheckProgress", Args JSONDouble 1 Nothing Nothing Nothing "Progress of recheck. 0.0 to 1.0."),
    ("secondsDownloading", Args JSONNumber 15 Nothing Nothing Nothing ""),
    ("secondsSeeding", Args JSONNumber 15 Nothing Nothing Nothing ""),
    ("seedIdleLimit", Args JSONNumber 10 Nothing Nothing Nothing "Idle limit in minutes."),
    ("seedIdleMode", Args JSONNumber 10 Nothing Nothing Nothing "Use global (0), torrent (1), or unlimited (2) limit."),
    ("seedRatioLimit", Args JSONDouble 5 Nothing Nothing Nothing "Seed ratio limit."),
    ("seedRatioMode", Args JSONNumber 5 Nothing Nothing Nothing "Use global (0), torrent (1), or unlimited (2) limit."),
    ("sizeWhenDone", Args JSONNumber 1 Nothing Nothing Nothing "Size of the torrent download in bytes."),
    ("startDate", Args JSONNumber 1 Nothing Nothing Nothing "The date when the torrent was last started."),
    ("status", Args JSONNumber 1 Nothing Nothing Nothing "Current status, see source"),
    ("trackers", Args JSONArray 1 Nothing Nothing Nothing "Array of tracker objects."),
    ("trackerStats", Args JSONObject 7 Nothing Nothing Nothing "Array of object containing tracker statistics."),
    ("totalSize", Args JSONNumber 1 Nothing Nothing Nothing "Total size of the torrent in bytes"),
    ("torrentFile", Args JSONString 5 Nothing Nothing Nothing "Path to .torrent file."),
    ("uploadedEver", Args JSONNumber 1 Nothing Nothing Nothing "Number of bytes uploaded, ever."),
    ("uploadLimit", Args JSONNumber 1 Nothing Nothing Nothing "Upload limit in Kbps"),
    ("uploadLimited", Args JSONBool 5 Nothing Nothing Nothing "Upload limit enabled."),
    ("uploadRatio", Args JSONDouble 1 Nothing Nothing Nothing "Seed ratio."),
    ("wanted", Args JSONArray 1 Nothing Nothing Nothing "Array of booleans indicated wanted files."),
    ("webseeds", Args JSONArray 1 Nothing Nothing Nothing "Array of webseeds objects"),
    ("webseedsSendingToUs", Args JSONNumber 1 Nothing Nothing Nothing "Number of webseeds seeding to us."),
    ("file-count", Args JSONNumber 17 Nothing Nothing Nothing ""),
    ("trackerList", Args
        JSONArray
        17 Nothing Nothing Nothing
        "A Iterable[Iterable[str]] for a set of announce URLs, each inner list is for a tier"
    )]
