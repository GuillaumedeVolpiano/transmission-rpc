{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Transmission.RPC.Client (
  -- * Reexports from Transmission.RPC.Types
  Client
  -- * Client methods
  , fromUrl
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
  )
where
import           Control.Lens                    ((.~))
import           Control.Lens.Operators          ((&), (^.))
import           Data.Aeson                      (ToJSON,
                                                  Value (Array, Null, Number, Object),
                                                  object, toJSON, fromJSON)
import qualified Data.Aeson as A (Value(String), Result(..))
import           Data.Aeson.Key                  (fromString)
import           Data.Aeson.KeyMap               (KeyMap)
import qualified Data.Aeson.KeyMap               as K (insert, lookup, member,
                                                       singleton)
import           Data.Aeson.Parser               (json)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as L (ByteString)
import           Data.Fixed                      (E3, Fixed, showFixed)
import           Data.Functor                    (void)
import qualified Data.HashSet                    as S (fromList)
import           Data.Maybe                      (catMaybes, fromMaybe,
                                                  mapMaybe)
import qualified Data.Text                       as T (pack, unpack)
import qualified Data.Vector                     as V (toList)
import           Effectful                       (Eff, (:>))
import           Effectful.Error.Static          (HasCallStack)
import           Effectful.Exception             (throwIO, try)
import           Effectful.FileSystem            (FileSystem)
import           Effectful.Log                   (Log, logAttention_, logInfo_)
import           Effectful.Reader.Static         (Reader, asks)
import           Effectful.Time                  (Time, monotonicTime)
import           Effectful.Wreq                  (Options, Response, Wreq,
                                                  header, manager, responseBody,
                                                  responseHeader,
                                                  responseStatus, statusCode, defaults)
import           Effectful.Wreq.Session          (Session, get, newSession,
                                                  postWith)
import           Network.HTTP.Client             (HttpException (HttpExceptionRequest),
                                                  HttpExceptionContent (StatusCodeException),
                                                  defaultManagerSettings,
                                                  managerResponseTimeout,
                                                  responseTimeoutMicro)
import           Transmission.RPC.Constants      (defaultTimeout,
                                                  sessionIdHeaderName)
import           Transmission.RPC.Errors         (TransmissionContext (..),
                                                  TransmissionError (..))
import           Transmission.RPC.Types          (Client (..), ID (..), Label,
                                                  RPCMethod (..), Timeout,
                                                  Torrent, TorrentRef, getOpts,
                                                  getSession, getURI, newClient, IDs (..))
import           Transmission.RPC.Utils          (getTorrentArguments,
                                                  maybeJSON, readTorrent)

-- constants
currentProtocolVersion :: Int
currentProtocolVersion = 17

-- | Create a client from a URL, a timeout and a Logger
fromUrl :: Wreq :> es => String -> Maybe Options -> Timeout -> Eff es Client
fromUrl url opts timeout =  do
                      sesh <- newSession
                      sessionId <- getSessionId url sesh
                      let timeout' = fromMaybe defaultTimeout timeout
                          opts' = fromMaybe defaults opts & header sessionIdHeaderName .~ [sessionId] & manager .~ Left (defaultManagerSettings {managerResponseTimeout = responseTimeoutMicro timeout'})
                      pure $ newClient url sesh opts' currentProtocolVersion


-- | Add a torrent to the transfer list
addTorrent :: (FileSystem :> es, Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => TorrentRef -> Timeout -> Maybe FilePath -> Maybe [Int] -> Maybe [Int] -> Maybe Bool -> Maybe Int -> Maybe [Int] -> Maybe [Int] -> Maybe [Int] -> Maybe String -> Maybe [Label] -> Maybe Int -> Eff es Value
addTorrent tref timeout downloadDir filesUnwanted filesWanted paused peerLimit priorityHigh priorityLow priorityNormal cookies labels bandwidthPriority= do
                                     torrentData <- readTorrent tref
                                     let args = Just . object . (torrentData :) . catMaybes $ [maybeJSON ("download-dir", downloadDir), maybeJSON ("files-unwanted", filesUnwanted), maybeJSON ("files-wanted", filesWanted), maybeJSON ("paused", paused), maybeJSON ("peerLimit", peerLimit), maybeJSON ("priority-high", priorityHigh), maybeJSON ("priority-low", priorityLow), maybeJSON ("priority-normal", priorityNormal), maybeJSON ("cookies", cookies), maybeJSON ("labels", labels), maybeJSON ("bandwidthPriority", bandwidthPriority)]
                                     request TorrentAdd args Nothing False timeout

-- | Remove torrent(s) with provided id(s). Local data will be removed by
-- transmission daemon if Bool is True
deleteTorrent :: (Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => IDs -> Bool -> Timeout -> Eff es ()
deleteTorrent ids deleteData = void . request TorrentRemove (Just $ object [(fromString "delete-local-data", toJSON deleteData)]) (Just ids) True

-- | Starttorrent(s) with provided id(s)
startTorrent :: (Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => IDs -> Bool -> Timeout -> Eff es ()
startTorrent ids bypassQueue = void . request (if bypassQueue then TorrentStartNow else TorrentStart) Nothing (Just ids) True

-- | Start all torrents respecting the queue order
startAll :: (Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => Bool -> Timeout -> Eff es ()
startAll bypassQueue timeout = do
  let method = if bypassQueue then TorrentStartNow else TorrentStart
      extractID Nothing = Nothing
      extractID (Just (Number n)) = Just . ID $ round n
      extractID (Just (A.String s)) = Just . Hash . T.unpack $ s
      extractID (Just v) = error ("Value " ++ show v ++ " is not a number")
  ids <- IDs . mapMaybe (extractID . K.lookup (fromString "id")) <$> getTorrents Nothing (Just []) Nothing
  void . request method Nothing (Just ids) True $ timeout

-- | Stop torrent(s) with provided id(s)
stopTorrent :: (Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => IDs -> Timeout -> Eff es ()
stopTorrent ids = void . request TorrentStop Nothing (Just ids) True

-- | Verify torrent(s) with provided id(s)
verifyTorrent :: (Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => IDs -> Timeout -> Eff es ()
verifyTorrent ids = void . request TorrentVerify Nothing (Just ids) True

-- | Reannounce torrent(s) with provided id(s)
reannounceTorrent :: (Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => IDs -> Timeout -> Eff es ()
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
getTorrent :: (Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => ID -> Maybe [String] -> Timeout -> Eff es Torrent
getTorrent iD fields timeout = do
  torrents <- getTorrents (Just . IDs $ [iD]) fields timeout
  case torrents of
    [] -> throwIO . TransmissionError $ TransmissionContext "No torrent returned" (Just TorrentGet) Nothing Nothing Nothing Nothing
    [t] -> pure t
    _ -> throwIO . TransmissionError $ TransmissionContext ("Received more than one torrent: " ++ show torrents) (Just TorrentGet) Nothing Nothing Nothing Nothing

-- | Get information for torrents with provided ids. For more information see Client.get_torrent().
getTorrents :: (Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => Maybe IDs -> Maybe [String] -> Timeout -> Eff es [Torrent]
getTorrents ids fields timeout = do
  args <- buildArguments fields
  response <- request TorrentGet (Just . object $ [(fromString "fields", args)]) ids False timeout
  let res = case response of
              Object r -> r
              _        -> error (show response ++ "is not an object")
  case K.lookup (fromString "torrents") res of
                                       Nothing -> throwIO . TransmissionError $ TransmissionContext "Response has no torrent field" (Just TorrentGet) (Just . object $ [(fromString "fields", args)]) (Just response) Nothing Nothing
                                       Just (Array tors) -> pure . map extractValues . V.toList $ tors
                                       Just _ -> throwIO . TransmissionError $ TransmissionContext "torrent field does not contain an Array" (Just TorrentGet) (Just . object $ [(fromString "fields", args)]) (Just response) Nothing Nothing

-- | Get information for torrents for recently active torrent. If you want to get recently-removed torrents. you should use this method.
-- Returns a list of active torrents and a list of ids of recently removed torrents
getRecentlyActiveTorrents :: (Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => Maybe [String] -> Timeout -> Eff es ([Torrent], IDs)
getRecentlyActiveTorrents fields timeout = do
  args <- buildArguments fields
  result <- request TorrentGet (Just . object $ [(fromString "fields", args)]) (Just RecentlyActive) False timeout
  let res = case result of
              Object r -> r
              _ -> error (show result ++ "is not an object")
      tors = case K.lookup (fromString "torrents") res of
               Nothing -> []
               Just (Array a) -> map extractValues . V.toList $ a
               Just _ -> error ("torrent field does not contain an Array " ++ show res)
      removed = case K.lookup (fromString "removed") res of 
                  Nothing -> []
                  Just a@(Array _) -> case fromJSON a of 
                                        A.Error s -> error s
                                        A.Success b -> b
                  Just _ -> error ("removed field does not contain an Array " ++ show res)
  pure (tors, IDs removed)

-- Utility functions

extractValues :: Value -> KeyMap Value
extractValues (Object v) = v
extractValues v = error (show v ++ "is not an object")

buildArguments :: (Reader Client :> es) => Maybe [String] -> Eff es Value
buildArguments fields = asks getProtocolVersion >>= \protocolVersion -> pure . maybe (toJSON . getTorrentArguments $ protocolVersion) (toJSON . S.fromList . ("id" :) . ("hashstring" :)) $ fields

getSessionId :: Wreq :> es => String -> Session -> Eff es ByteString
getSessionId url sesh = do
                failGet <- try (get sesh url)
                let sessionId (Right r) = throwIO . TransmissionConnectError $ TransmissionContext "Error why getting X-Session-Id" Nothing Nothing Nothing (Just r) Nothing
                    sessionId (Left e@(HttpExceptionRequest _ (StatusCodeException s _)))
                                  | s ^. (responseStatus . statusCode) == 409 = pure $ s ^. responseHeader sessionIdHeaderName
                                  | otherwise = throwIO e
                    sessionId (Left e) = throwIO e
                sessionId failGet


request :: (HasCallStack, Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => RPCMethod -> Maybe Value -> Maybe IDs -> Bool -> Timeout -> Eff es Value
request _ _ Nothing True _ = error "request requires ids"
request _ _ (Just (IDs [])) True _ = error "request requires ids"
request rpcm args ids _ timeout = do
  let args' = maybe id (valueInsert "ids") ids . fromMaybe Null $ args
      query = object [(fromString "method", toJSON rpcm), (fromString "arguments", args')]
  sesh <- asks getSession
  opts <- asks getOpts
  let opts' = maybe opts (\t -> opts & manager .~ Left (defaultManagerSettings {managerResponseTimeout = responseTimeoutMicro t})) timeout
  uri <- asks getURI
  start <- monotonicTime
  response <- postWith opts' sesh uri query
  elapsed <- (+ negate start) <$> monotonicTime
  logAttention_ (T.pack $ "http request took " ++ showFixed True (fromRational . toRational $ elapsed :: Fixed E3) ++ " s")
  let body = parse json $ response ^. responseBody
  examineBody body rpcm query response

examineBody :: Log :> es => Result Value -> RPCMethod -> Value -> Response L.ByteString -> Eff es Value
examineBody (Fail _ _ e) rpcm query response = logInfo_ (T.pack $ "Error:\n" ++ "Request: " ++ show query ++ "\n" ++ "HTTP Data: " ++  show response) >> throwIO (TransmissionError (TransmissionContext ("failed to parse response as JSON:\n" ++ show e) (Just rpcm ) (Just query) Nothing (Just response) Nothing))
examineBody (Done _ jsonBody@(Object bodyMap)) rpcm query response = case K.lookup (fromString "result") bodyMap of
                                        Nothing -> throwIO . TransmissionError $ TransmissionContext "Query failed, response data missing result:\n" (Just rpcm) (Just query) (Just jsonBody) (Just response) Nothing
                                        Just result -> checkSuccess result
   where
     checkSuccess success
       | success == toJSON "success" =
              case K.lookup (fromString "arguments") bodyMap of
                Just value@(Object res) -> do
                    case rpcm of
                      TorrentGet        -> pure value
                      TorrentAdd        -> added res
                      SessionGet        -> rawSessionUpdate res
                      SessionStats      ->  undefined
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
                          | K.member (fromString "torrent-added") res = K.lookup (fromString "torrent-added") res
                          | K.member (fromString "torrent-duplicate") res = K.lookup (fromString "torrent-duplicate") res
                          | otherwise = Nothing
                       result = fromMaybe Null item
                   pure result

examineBody (Done _ r) _ query response = logInfo_ (T.pack $ "Error:\n" ++ "Request: " ++ show query ++ "\n" ++ "HTTP Data: " ++  show response) >> error ("response is not an Object: " ++ show r)

rawSessionUpdate :: a
rawSessionUpdate = undefined

valueInsert :: ToJSON a => String -> a -> Value -> Value
valueInsert key value (Object km ) = Object . K.insert (fromString key) (toJSON value) $ km
valueInsert key value Null = Object . K.singleton (fromString key) $ toJSON value
valueInsert key value v = error (show v ++ " is not an Object and not Null, cannot insert value " ++ (show . toJSON $ value) ++ " for key " ++ key)
