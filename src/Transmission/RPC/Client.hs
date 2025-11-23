{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Transmission.RPC.Client (
  -- * Reexports from Transmission.RPC.Types
  Client
  -- * Client methods
  , addTorrent
  , deleteTorrent
  , startTorrent
  , startAll
  , stopTorrent
  , verifyTorrent
  , reannounceTorrent
  , getTorrent
  , getTorrents
  , getRecentlyActiveTorrents
  , changeTorrents
  , moveTorrentData
  , renameTorrentPath
  , queueTop
  , queueBottom
  , queueUp
  , queueDown
  , getSession
  , setSession
  , blocklistUpdate
  , portTest
  , freeSpace
  , sessionStats
  , setGroup
  , groupGet
  , groupsGet
  )
where
import           Control.Applicative                ((<|>))
import           Data.Aeson                         (FromJSON, Key, ToJSON,
                                                     Value (Array, Null, Object),
                                                     fromJSON, object, toJSON,
                                                     (.=), encode)
import qualified Data.Aeson                         as A (Result (..))
import           Data.Aeson.Key                     (fromString)
import           Data.Aeson.KeyMap                  (KeyMap)
import qualified Data.Aeson.KeyMap                  as K (insert, lookup,
                                                          member, singleton)
import           Data.Aeson.Parser                  (json)
import           Data.Attoparsec.ByteString.Lazy    (Result (..), parse)
import qualified Data.ByteString.Base64             as B64 (encode)
import qualified Data.ByteString.Lazy               as L (ByteString)
import           Data.Fixed                         (E3, Fixed, showFixed)
import           Data.Functor                       (void)
import qualified Data.HashSet                       as S (fromList)
import           Data.List                          (intersperse)
import           Data.Map                           (Map, (!))
import           Data.Maybe                         (catMaybes, fromJust,
                                                     fromMaybe)
import           Data.Text                          (Text)
import qualified Data.Text                          as T (pack)
import qualified Data.Text.Encoding                 as TE (decodeUtf8)
import           Data.Text.Encoding.Error           (UnicodeException (DecodeError))
import qualified Data.Vector                        as V (head, toList)
import           Effectful                          (Eff, (:>))
import           Effectful.Error.Static             (HasCallStack)
import           Effectful.Exception                (fromException, throwIO,
                                                     try)
import           Effectful.FileSystem               (FileSystem)
import           Effectful.FileSystem.IO.ByteString (hGetContents)
import qualified Effectful.FileSystem.IO.ByteString as FS
import           Effectful.Log                      (Log, logAttention_,
                                                     logInfo_)
import           Effectful.Time                     (Time, monotonicTime)
import           Network.HTTP.Client                (HttpException (HttpExceptionRequest),
                                                     HttpExceptionContent (ResponseTimeout),
                                                     responseTimeoutMicro, Response, Request, responseTimeout, parseRequest_, requestBody, requestHeaders, RequestBody (..), responseStatus, responseHeaders, responseBody)
import qualified Network.HTTP.Client                as HC (method)
import           Transmission.RPC.Constants         (Priority,
                                                     sessionIdHeaderName)
import           Transmission.RPC.Enum              (EncryptionMode, IdleMode,
                                                     RatioLimitMode)
import           Transmission.RPC.Errors            (TransmissionContext (..),
                                                     TransmissionError (..))
import qualified Transmission.RPC.Session           as TS (Session)
import           Transmission.RPC.Session           (SessionStats, rpcVersion,
                                                     rpcVersionSemver, version)
import           Transmission.RPC.Torrent           (Torrent, mkTorrent, toId)
import           Transmission.RPC.Types             (ID (..), IDs (..), Label,
                                                     RPCMethod (..), Timeout,
                                                     TorrentRef (..), URI)
import           Transmission.RPC.Utils             (getTorrentArguments)
import Effectful.Network.HTTP.Client (HttpClient, httpLbs, setHeaders)
import Effectful.RPC.Client (Client, getProtocolVersion, getURI, getClientSession, setClientSession, getServerVersion, getSemVerVersion, setProtocolVersion, setServerVersion, setSemVerVersion)
import Network.HTTP.Types (statusCode)

-- | Add a torrent to the transfer list
addTorrent :: (FileSystem :> es, HttpClient :> es, Client :> es, Log :> es, Time :> es) => TorrentRef -> Timeout -> Maybe FilePath -> Maybe [Int] -> Maybe [Int] -> Maybe Bool -> Maybe Int -> Maybe [Int] -> Maybe [Int] -> Maybe [Int] -> Maybe Text -> Maybe [Label] -> Maybe Int -> Eff es Torrent
addTorrent tref timeout downloadDir filesUnwanted filesWanted paused peerLimit priorityHigh priorityLow priorityNormal cookies labels bandwidthPriority = do
  torrentData <- (
                  case tref of
                      TorrentURI u -> pure $ "filename" .= u
                      Binary b -> ("metainfo" .=) . TE.decodeUtf8 . B64.encode <$> hGetContents b
                      TorrentContent c -> pure $ "metainfo" .= (TE.decodeUtf8 . B64.encode $ c)
                      Path p -> ("metainfo" .=) . TE.decodeUtf8 . B64.encode <$> FS.readFile p
                 ) :: (FileSystem :> es0) => Eff es0 (Key, Value)
  let args = Just . object . (torrentData :) . catMaybes $ [("download-dir" .=) <$> downloadDir
                                                           , ("files-unwanted" .=) <$> filesUnwanted
                                                           , ("files-wanted" .=) <$> filesWanted
                                                           , ("paused" .=) <$> paused
                                                           , ("peerLimit" .=) <$> peerLimit
                                                           , ("priority-high" .=) <$> priorityHigh
                                                           , ("priority-low" .=) <$> priorityLow
                                                           , ("priority-normal" .=) <$> priorityNormal
                                                           , ("cookies" .=) <$> cookies
                                                           , ("labels" .=) <$> labels
                                                           , ("bandwidthPriority" .=) <$> bandwidthPriority]
  mkTorrent <$> request TorrentAdd args Nothing False timeout

-- | Remove torrent(s) with provided id(s). Local data will be removed by
-- transmission daemon if Bool is True
deleteTorrent :: (HttpClient :> es, Client :> es, Log :> es, Time :> es) => IDs -> Bool -> Timeout -> Eff es ()
deleteTorrent ids deleteData = void . request TorrentRemove (Just $ object [("delete-local-data", toJSON deleteData)]) (Just ids) True

-- | Starttorrent(s) with provided id(s)
startTorrent :: (HttpClient :> es, Client :> es, Log :> es, Time :> es) => IDs -> Bool -> Timeout -> Eff es ()
startTorrent ids bypassQueue = void . request (if bypassQueue then TorrentStartNow else TorrentStart) Nothing (Just ids) True

-- | Start all torrents respecting the queue order
startAll :: (HttpClient :> es, Client :> es, Log :> es, Time :> es) => Bool -> Timeout -> Eff es ()
startAll bypassQueue timeout = do
  let method = if bypassQueue then TorrentStartNow else TorrentStart
  ids <- IDs . map (ID . fromJust . toId) <$> getTorrents Nothing (Just []) Nothing
  void . request method Nothing (Just ids) True $ timeout

-- | Stop torrent(s) with provided id(s)
stopTorrent :: (HttpClient :> es, Client :> es, Log :> es, Time :> es) => IDs -> Timeout -> Eff es ()
stopTorrent ids = void . request TorrentStop Nothing (Just ids) True

-- | Verify torrent(s) with provided id(s)
verifyTorrent :: (HttpClient :> es, Client :> es, Log :> es, Time :> es) => IDs -> Timeout -> Eff es ()
verifyTorrent ids = void . request TorrentVerify Nothing (Just ids) True

-- | Reannounce torrent(s) with provided id(s)
reannounceTorrent :: (HttpClient :> es, Client :> es, Log :> es, Time :> es) => IDs -> Timeout -> Eff es ()
reannounceTorrent ids = void . request TorrentReannounce Nothing (Just ids) True

-- | Get information for torrent with provided id. arguments contains a list of field names to be returned, when arguments=None (default), all fields are requested. See the Torrent class for more information.
-- new argument format in rpc_version 16 is unnecessarily and this lib can’t handle table response, So it’s unsupported.
-- Returns a Torrent object with the requested fields.
-- Note:
--            It's recommended that you only fetch arguments you need,
--            this could improve response speed.
--
--            For example, fetch all fields from transmission daemon with 1500 torrents would take ~5s,
--            but is only ~0.2s if to fetch 6 fields.
getTorrent :: (HttpClient :> es, Client :> es, Log :> es, Time :> es) => ID -> Maybe [Text] -> Timeout -> Eff es Torrent
getTorrent toID fields timeout = do
  torrents <- getTorrents (Just . IDs $ [toID]) fields timeout
  case torrents of
    [] -> throwIO . TransmissionError $ TransmissionContext "No torrent returned" (Just TorrentGet) Nothing Nothing Nothing Nothing
    [t] -> pure t
    _ -> throwIO . TransmissionError $ TransmissionContext ("Received more than one torrent: " ++ show torrents) (Just TorrentGet) Nothing Nothing Nothing Nothing

-- | Get information for torrents with provided ids. For more information see Client.get_torrent().
getTorrents :: (HttpClient :> es, Client :> es, Log :> es, Time :> es) => Maybe IDs -> Maybe [Text] -> Timeout -> Eff es [Torrent]
getTorrents ids fields timeout = do
  args <- buildArguments fields
  response <- request TorrentGet (Just . object $ [("fields", args)]) ids False timeout
  let res = case response of
              Object r -> r
              _        -> error (show response ++ "is not an object")
  case K.lookup "torrents" res of
                                       Nothing -> throwIO . TransmissionError $ TransmissionContext "Response has no torrent field" (Just TorrentGet) (Just . object $ [("fields", args)]) (Just response) Nothing Nothing
                                       Just (Array tors) -> pure . map mkTorrent . V.toList $ tors
                                       Just _ -> throwIO . TransmissionError $ TransmissionContext "torrent field does not contain an Array" (Just TorrentGet) (Just . object $ [("fields", args)]) (Just response) Nothing Nothing

-- | Get information for torrents for recently active torrent. If you want to get recently-removed torrents. you should use this method.
-- Returns a list of active torrents and a list of ids of recently removed torrents
getRecentlyActiveTorrents :: (HttpClient :> es, Client :> es, Log :> es, Time :> es) => Maybe [Text] -> Timeout -> Eff es ([Torrent], IDs)
getRecentlyActiveTorrents fields timeout = do
  args <- buildArguments fields
  result <- request TorrentGet (Just . object $ [("fields", args)]) (Just RecentlyActive) False timeout
  let res = case result of
              Object r -> r
              _        -> error (show result ++ "is not an object")
      tors = case K.lookup "torrents" res of
               Nothing -> []
               Just (Array a) -> map mkTorrent . V.toList $ a
               Just _ -> error ("torrent field does not contain an Array " ++ show res)
      removed = case K.lookup "removed" res of
                  Nothing -> []
                  Just a@(Array _) -> case fromJSON a of
                                        A.Error s   -> error s
                                        A.Success b -> b
                  Just _ -> error ("removed field does not contain an Array " ++ show res)
  pure (tors, IDs removed)

-- | change torrent(s) parameters for the torrents with the supplied id(s)
changeTorrents :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => IDs -> Timeout -> Maybe Priority -> Maybe Int -> Maybe Bool -> Maybe Int -> Maybe Bool -> Maybe [Int] -> Maybe [Int] -> Maybe Bool -> Maybe FilePath -> Maybe Int -> Maybe [Priority] -> Maybe [Priority] -> Maybe [Priority] -> Maybe Int -> Maybe Int -> Maybe IdleMode -> Maybe Rational -> Maybe RatioLimitMode -> Maybe [Text] -> Maybe Text -> Maybe Text -> Maybe [[Text]] -> Maybe [(Int, Text)] -> Maybe [Int] -> [(Key, Value)] -> Eff es ()
changeTorrents ids timeout bandwidthPriority downloadLimit downloadLimited uploadLimit uploadLimited filesUnwanted filesWanted honorsSessionsLimits location peerLimit priorityHigh priorityLow priorityNormal queuePosition
  seedIdleLimit seedIdleMode seedRatioLimit seedRatioMode trackerAdd labels group trackerList trackerReplace trackerRemove additionalArgs = do
    let args = (additionalArgs ++) . catMaybes $ [("bandwidthPriority" .=) <$> bandwidthPriority, ("downloadLimit" .=) <$> downloadLimit, ("downloadLimited" .=) <$> downloadLimited, ("uploadLimit" .=) <$> uploadLimit,
                                    ("uploadLimited" .=) <$> uploadLimited, ("files-unwanted" .=) <$> filesUnwanted, ("files-wanted" .=) <$> filesWanted, ("honorsSessionsLimits" .=) <$> honorsSessionsLimits,
                                    ("location" .=) <$> location, ("peer-limit" .=) <$> peerLimit, ("priority-high" .=) <$> priorityHigh, ("priority-low" .=) <$> priorityLow, ("priority-normal" .=) <$> priorityNormal,
                                    ("queuePosition" .=) <$> queuePosition, ("seedIdleLimit".=) <$> seedIdleLimit, ("seedIdleMode" .=) <$> seedIdleMode, ("seedRatioLimit" .=) <$> seedRatioLimit,
                                    ("seedRatioMode" .=) <$> seedRatioMode, ("trackerAdd" .=) <$> trackerAdd, ("labels" .=) <$> labels, ("group" .=) <$> group, ("trackerList" .=) <$> trackerList,
                                    ("trackerReplace" .=) <$> trackerReplace, ("trackerRemove" .=) <$> trackerRemove]

    if null args then error "No arguments to set"
                 else void $ request TorrentSet (Just . object $ args) (Just ids) True timeout


-- | Move torrent data to a new location
moveTorrentData :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => IDs -> FilePath -> Timeout -> Bool -> Eff es ()
moveTorrentData ids location timeout move = do
  let args = object ["location" .= location, "move" .= move]
  void $ request TorrentSetLocation (Just args) (Just ids) True timeout

-- | Rename the path of a torrent, doesn't move it.
renameTorrentPath :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => ID -> FilePath -> Text -> Timeout -> Eff es (Text, Text)
renameTorrentPath toID location name timeout = do
  result <- request TorrentRenamePath (Just . object $ ["path" .= location, "name" .= name]) (Just . IDs $ [toID]) True timeout
  case (fromJSON result :: A.Result (Map Text Text)) of
    A.Error s    -> error s
    A.Success km -> pure (km ! "path", km ! "name")

-- | Move transfer to the top of the queue
queueTop :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => IDs -> Timeout -> Eff es ()
queueTop ids timeout = void $ request QueueMoveTop Nothing (Just ids) True timeout

-- | Move transfer to the bottom of the queue
queueBottom :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => IDs -> Timeout -> Eff es ()
queueBottom ids timeout = void $ request QueueMoveBottom Nothing (Just ids) True timeout

-- | Move transfer up in the queue
queueUp :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => IDs -> Timeout -> Eff es ()
queueUp ids timeout = void $ request QueueMoveUp Nothing (Just ids) True timeout

-- | Move transfer down in the queue
queueDown :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => IDs -> Timeout -> Eff es ()
queueDown ids timeout = void $ request QueueMoveDown Nothing (Just ids) True timeout

-- | Get Session parameters
getSession :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => Maybe [Text] -> Timeout -> Eff es TS.Session
getSession fields timeout = do
  response <- request SessionGet ((\f -> object [("fields", toJSON f)]) <$> fields) Nothing False timeout
  case fromJSON response of
    A.Error s   -> error s
    A.Success s -> updateVersions >> pure s

-- | Set Session parameters
setSession :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => Timeout -> Maybe Int -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Int -> Maybe Bool -> Maybe [Text] -> Maybe FilePath -> Maybe Bool -> Maybe Int -> Maybe EncryptionMode -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Bool -> Maybe Int -> Maybe Rational -> Maybe Bool -> Maybe Int -> Maybe Bool -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe FilePath -> Maybe Bool -> Maybe Bool -> Maybe FilePath-> [(Key, Value)] -> Eff es ()
setSession timeout altSpeedDown altSpeedEnabled altSpeedTimeBegin altSpeedTimeDay altSpeedTimeEnabled altSpeedTimeEnd altSpeedUp blocklistEnabled blockListURL cacheSizeMB dhtEnabled defaultTrackers downloadDir downloadQueueEnabled downloadQueueSize encryption idleSeedingLimit idleSeedingLimitEnabled incompleteDir incompleteDirEnabled lpdEnabled peerLimitGlobal peerLimitPerTorrent peerPort peerPortRandomOnStart pexEnabled portForwardingEnabled queueStalledEnabled queueStalledMinutes renamePartialFiles scriptTorrentDoneEnabled scriptTorrentDoneFilename seedQueueEnabled seedQueueSize seedRatioLimit seedRatioLimited speedLimitDown speedLimitDownEnabled speedLimitUp speedLimitUpEnabled startAddedTorrents trashOriginalTorrentFile utpEnabled scriptTorrentDoneSeedingFilename scriptTorrentDoneSeedingEnabled scriptTorrentAddedEnabled scriptTorrentAddedFilename additionalArgs = do
    let args = (additionalArgs ++) . catMaybes $ [("alt-speed-down" .=) <$> altSpeedDown
                                                 , ("alt-speed-enabled" .=) <$> altSpeedEnabled
                                                 , ("alt-speed-time-begin" .=) <$> altSpeedTimeBegin
                                                 , ("alt-speed-time-day" .=) <$> altSpeedTimeDay
                                                 , ("alt-speed-time-enabled" .=) <$> altSpeedTimeEnabled
                                                 , ("alt-speed-time-end" .=) <$> altSpeedTimeEnd
                                                 , ("alt-speed-up" .=) <$> altSpeedUp
                                                 , ("blocklist-enabled" .=) <$> blocklistEnabled
                                                 , ("blocklist-url" .=) <$> blockListURL
                                                 , ("cache-size-mb" .=) <$> cacheSizeMB
                                                 , ("default-trackers" .=) . intersperse "\n" <$> defaultTrackers
                                                 , ("dht-enabled" .=) <$> dhtEnabled
                                                 , ("download-dir" .=) <$> downloadDir
                                                 , ("download-queue-enabled" .=) <$> downloadQueueEnabled
                                                 , ("download-queue-size" .=) <$> downloadQueueSize
                                                 , ("encryption" .=) <$> encryption
                                                 , ("idle-seeding-limit" .=) <$> idleSeedingLimit
                                                 , ("idle-seeding-limit-enabled" .=) <$> idleSeedingLimitEnabled
                                                 , ("incomplete-dir" .=) <$> incompleteDir
                                                 , ("incomplete-dir-enabled" .=) <$> incompleteDirEnabled
                                                 , ("lpd-enabled" .=) <$> lpdEnabled
                                                 , ("peer-limit-global" .=) <$> peerLimitGlobal
                                                 , ("peer-limit-per-torrent" .=) <$> peerLimitPerTorrent
                                                 , ("peer-port" .=) <$> peerPort
                                                 , ("peer-port-random-on-start" .=) <$> peerPortRandomOnStart
                                                 , ("pex-enabled" .=) <$> pexEnabled
                                                 , ("port-forwarding-enabled" .=) <$> portForwardingEnabled
                                                 , ("queue-stalled-enabled" .=) <$> queueStalledEnabled
                                                 , ("queue-stalled-minutes" .=) <$> queueStalledMinutes
                                                 , ("rename-partial-files" .=) <$> renamePartialFiles
                                                 , ("script-torrent-done-enabled" .=) <$> scriptTorrentDoneEnabled
                                                 , ("script-torrent-done-filename" .=) <$> scriptTorrentDoneFilename
                                                 , ("seed-queue-enabled" .=) <$> seedQueueEnabled
                                                 , ("seed-queue-size" .=) <$> seedQueueSize
                                                 , ("seedRatiorLimit" .=) <$> seedRatioLimit
                                                 , ("seedRatioLimited" .=) <$> seedRatioLimited
                                                 , ("speed-limit-down" .=) <$> speedLimitDown
                                                 , ("speed-limit-down-enabled" .=) <$> speedLimitDownEnabled
                                                 , ("speed-limit-up" .=) <$> speedLimitUp
                                                 , ("speed-limit-up-enabled" .=) <$> speedLimitUpEnabled
                                                 , ("start-added-torrents" .=) <$> startAddedTorrents
                                                 , ("trash-original-torrent-file" .=) <$> trashOriginalTorrentFile
                                                 , ("utp-enabled" .=) <$> utpEnabled
                                                 , ("script-torrent-done-seeding-filename" .=) <$> scriptTorrentDoneSeedingFilename
                                                 , ("script-torrent-done-seeding-enabled" .=) <$> scriptTorrentDoneSeedingEnabled
                                                 , ("script-torrent-added-enabled" .=) <$> scriptTorrentAddedEnabled
                                                 , ("script-torrent-added-filename" .=) <$> scriptTorrentAddedFilename]
    if null args then error "No arguments to set"
                 else void $ request TorrentSet (Just . object $ args) Nothing False timeout

-- | Update blocklist. Returns the size of the blocklist

blocklistUpdate :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => Timeout -> Eff es Int
blocklistUpdate timeout = do
  result <- request BlocklistUpdate Nothing Nothing False timeout
  case fromJSON result of
    A.Error s   -> error s
    A.Success v -> pure v

-- | Tests to see if the incoming peer port is accessible from the outside world.

portTest :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => Timeout -> Eff es Bool
portTest timeout = extractFieldFromValue "port-is-open" <$> request PortTest Nothing Nothing False timeout

-- | Get the amount of free space (in bytes) at the provided location.

freeSpace :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => FilePath -> Timeout -> Eff es Int
freeSpace fp timeout = extractFieldFromValue "size-bytes" <$> request FreeSpace (Just . object $ [("path", toJSON fp)]) Nothing False timeout

-- | Get Session statistics

sessionStats :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => Timeout -> Eff es SessionStats
sessionStats timeout = do
  result <- request SessionStats Nothing Nothing False timeout
  case fromJSON result of
    A.Error s   -> error s
    A.Success s -> pure s

-- | create or update a Bandwidth group

setGroup :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => Text -> Timeout -> Maybe Bool -> Maybe Bool -> Maybe Int -> Maybe Bool -> Maybe Int -> Eff es ()
setGroup name timeout honorsSessionLimits speedLimitDownEnabled speedLimitDown speedLimitUpEnabled speedLimitUp =
  do
    let args = object . catMaybes $ [Just ("name" .= name), ("honorsSessionLimits" .= ) <$> honorsSessionLimits
                                    , ("speed-limit-down-enabled" .=) <$> speedLimitDownEnabled
                                    , ("speed-limit-down" .=) <$> speedLimitDown
                                    , ("speed-limitUpEnabled" .=) <$> speedLimitUpEnabled
                                    , ("speedLimitUp" .=) <$> speedLimitUp]
    void $ request GroupSet (Just args) Nothing False timeout

-- | Access Infos about a Bandwidth Group

groupGet :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => Text -> Timeout -> Eff es Value
groupGet name timeout = V.head . extractFieldFromValue "groups" <$> request GroupGet (Just . toJSON $ name) Nothing False timeout

-- | Access infos about a list of Bandwidth groups or all of them (if the list is empty)
groupsGet :: ( Client :> es, HttpClient :> es, Log :> es, Time :> es) => [Text] -> Timeout -> Eff es Value
groupsGet names timeout = extractFieldFromValue "groups" <$> request GroupGet (Just . toJSON $ names) Nothing False timeout

-- Utility functions

buildArguments :: ( Client :> es) => Maybe [Text] -> Eff es Value
buildArguments fields = getProtocolVersion >>= (\ protocolVersion
        -> pure
             . maybe
                 (toJSON . getTorrentArguments $ protocolVersion)
                 (toJSON . S.fromList . ("id" :) . ("hashstring" :))
             $ fields)

makeJSONPost :: URI -> Value -> Request
makeJSONPost uri value = req {requestBody = RequestBodyLBS (encode value), HC.method = "POST", requestHeaders = [("Content-Type", "application/json")]}
  where req = parseRequest_ uri

safeRequest :: ( HttpClient :> es, Client :> es, Log :> es) => Value -> Timeout -> Eff es (Response L.ByteString)
safeRequest query timeout = do
  uri <- getURI
  let baseRequest = makeJSONPost uri query
      fullRequest = maybe baseRequest (\t -> baseRequest {responseTimeout = responseTimeoutMicro t}) timeout
  unsafePost <- try . httpLbs $ fullRequest
  case unsafePost of
    Right r -> 
      case statusCode . responseStatus $ r of
        200 -> pure r
        409 -> do
            let sessionId = lookup sessionIdHeaderName . responseHeaders $ r
            case sessionId of
              Nothing -> do
                let tc = TransmissionConnectError $ TransmissionContext "Could not find session id" Nothing 
                          (Just query) Nothing (Just r) (Just fullRequest)
                throwIO tc
              Just v -> do 
                setHeaders [(sessionIdHeaderName, v)]
                safeRequest query timeout
        sc -> do
          let tc = TransmissionConnectError $ TransmissionContext ("Error " ++ show sc) Nothing (Just query) 
                    Nothing (Just r) (Just fullRequest)
          throwIO tc
    Left e -> do
      logAttention_ . T.pack $ "Error " ++ show e
      case fromException e of
        Just (HttpExceptionRequest _ ResponseTimeout) -> logInfo_ "Response Timeout" >> safeRequest query timeout
        Just e'@(HttpExceptionRequest _ _) -> (logAttention_ . T.pack $
          "Other Http Exception" ++ show e') >> throwIO e'
        _ -> case fromException e of
               Just (DecodeError desc mByte) -> do
                 let logMessageError = T.pack $ "Unicode decode error " ++ desc ++ ", problematic byte" ++ show mByte
                 logInfo_ logMessageError
                 throwIO e
               _ -> do
                 logAttention_ . T.pack $ show e
                 throwIO e


request :: (HasCallStack, HttpClient :> es, Client :> es, Log :> es, Time :> es) => RPCMethod -> Maybe Value -> Maybe IDs -> Bool -> Timeout -> Eff es Value
request _ _ Nothing True _ = error "request requires ids, received nothing"
request _ _ (Just (IDs [])) True _ = error "request requires ids, received empty list"
request rpcm args ids _ timeout = do
  let args' = maybe id (valueInsert "ids") ids . fromMaybe Null $ args
      query = object [("method", toJSON rpcm), ("arguments", args')]
  start <- monotonicTime
  response <- safeRequest query timeout
  elapsed <- (+ negate start) <$> monotonicTime
  logAttention_ (T.pack $ "http request took " ++ showFixed True (fromRational . toRational $ elapsed :: Fixed E3) ++ " s")
  let body = parse json $ responseBody response
  examineBody body rpcm query response

examineBody :: (Client :> es, Log :> es) => Result Value -> RPCMethod -> Value -> Response L.ByteString -> Eff es Value
examineBody (Fail _ _ e) rpcm query response = logInfo_ (T.pack $ "Error:\n" ++ "Request: " ++ show query ++ "\n" ++ "HTTP Data: " ++  show response) >> throwIO (TransmissionError (TransmissionContext ("failed to parse response as JSON:\n" ++ show e) (Just rpcm ) (Just query) Nothing (Just response) Nothing))
examineBody (Done _ jsonBody@(Object bodyMap)) rpcm query response = case K.lookup "result" bodyMap of
                                        Nothing -> throwIO . TransmissionError $ TransmissionContext "Query failed, response data missing result:\n" (Just rpcm) (Just query) (Just jsonBody) (Just response) Nothing
                                        Just result -> checkSuccess result
   where
     checkSuccess success
       | success == toJSON ("success" :: String) =
              case K.lookup "arguments" bodyMap of
                Just value@(Object res) -> do
                    case rpcm of
                      TorrentGet        -> pure value
                      TorrentAdd        -> added res
                      SessionGet        -> updateSession value >> pure value
                      SessionStats      -> pure value
                      PortTest          -> pure value
                      BlocklistUpdate   -> pure value
                      FreeSpace         -> pure value
                      TorrentRenamePath -> pure value
                      _                 -> pure Null
                _ -> throwIO . TransmissionError $ TransmissionContext "arguments is not an Object" (Just rpcm) (Just query) (Just jsonBody) (Just response) Nothing
      | otherwise = throwIO . TransmissionError $ TransmissionContext ("Query failed with result " ++ show success) (Just rpcm) (Just query) (Just jsonBody) (Just response) Nothing
     added :: KeyMap Value -> Eff es Value
     added res = do
                   let item
                          | K.member "torrent-added" res = K.lookup "torrent-added" res
                          | K.member "torrent-duplicate" res = K.lookup "torrent-duplicate" res
                          | otherwise = Nothing
                       result = fromMaybe Null item
                   pure result

examineBody (Done _ r) _ query response = logInfo_ (T.pack $ "Error:\n" ++ "Request: " ++ show query ++ "\n" ++ "HTTP Data: " ++  show response) >> error ("response is not an Object: " ++ show r)

valueInsert :: ToJSON a => String -> a -> Value -> Value
valueInsert key value (Object km ) = Object . K.insert (fromString key) (toJSON value) $ km
valueInsert key value Null = Object . K.singleton (fromString key) $ toJSON value
valueInsert key value v = error (show v ++ " is not an Object and not Null, cannot insert value " ++ (show . toJSON $ value) ++ " for key " ++ key)

updateSession :: (Client :> es) => Value -> Eff es ()
updateSession v = do
  let newSesh = case fromJSON v of
                  A.Error s   -> error s
                  A.Success s -> s
  setClientSession newSesh 

updateVersions :: ( Client :> es) => Eff es ()
updateVersions = do
  sesh <- getClientSession
  pv <- getProtocolVersion
  srv <- getServerVersion
  smv <- getSemVerVersion
  setProtocolVersion (fromMaybe pv $ rpcVersion sesh)
  setServerVersion (version sesh <|> srv)
  setSemVerVersion (rpcVersionSemver sesh <|> smv)

extractFieldFromValue :: FromJSON a => Key -> Value -> a
extractFieldFromValue field result = extract
  where
    res = case result of
            Object r -> r
            _        -> error (show result ++ " is not an object")
    extract = case K.lookup field res of
                Nothing -> error (show result ++ " has no field called " ++ show field)
                Just val -> case fromJSON val of
                              A.Error s   -> error s
                              A.Success b -> b
