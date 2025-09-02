{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Transmission.RPC.Client (
  -- * Reexports from Transmission.RPC.Types
  Client
  -- * Client methods
  , fromUrl
  , addTorrent
  )
where
import           Control.Lens                    ((.~))
import           Control.Lens.Operators          ((&), (^.))
import           Data.Aeson                      (Value (Null, Object), object,
                                                  toJSON)
import qualified Data.Aeson                      as A (Value (String))
import           Data.Aeson.Key                  (fromString, fromText)
import           Data.Aeson.KeyMap               (KeyMap)
import qualified Data.Aeson.KeyMap               as K (empty, lookup, member,
                                                       singleton)
import           Data.Aeson.Parser               (json)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as L (ByteString)
import           Data.Fixed                      (E3, Fixed, showFixed)
import           Data.Maybe                      (catMaybes, fromJust,
                                                  fromMaybe)
import qualified Data.Text                       as T (pack)
import           Effectful                       (Eff, (:>))
import           Effectful.Error.Static          (HasCallStack)
import           Effectful.Exception             (try, throwIO)
import           Effectful.FileSystem            (FileSystem)
import           Effectful.Log                   (Log, logAttention_, logInfo_)
import           Effectful.Reader.Static         (Reader, asks)
import           Effectful.Time                  (Time, monotonicTime)
import           Effectful.Wreq                  (Options, Response, Wreq,
                                                  header, manager, responseBody,
                                                  responseHeader,
                                                  responseStatus, statusCode)
import           Effectful.Wreq.Session          (Session, get, newSession,
                                                  postWith)
import           Network.HTTP.Client             (HttpException (HttpExceptionRequest),
                                                  HttpExceptionContent (StatusCodeException),
                                                  defaultManagerSettings,
                                                  managerResponseTimeout,
                                                  responseTimeoutMicro)
import           Transmission.RPC.Constants      (defaultTimeout,
                                                  sessionIdHeaderName)
import           Transmission.RPC.Types          (Client, ID, Label,
                                                  RPCMethod (..), Timeout,
                                                  TorrentRef, getOpts,
                                                  getSession, getURI, newClient)
import           Transmission.RPC.Utils          (maybeJSON, readTorrent)
import Transmission.RPC.Errors (TransmissionError (..), TransmissionContext (..))

-- | Create a client from a URL, a timeout and a Logger
fromUrl :: Wreq :> es => String -> Options -> Timeout -> Eff es Client
fromUrl url opts timeout =  do
                      sesh <- newSession
                      sessionId <- getSessionId url sesh
                      let timeout' = fromMaybe defaultTimeout timeout
                          opts' = opts & header sessionIdHeaderName .~ [sessionId] & manager .~ Left (defaultManagerSettings {managerResponseTimeout = responseTimeoutMicro timeout'})
                      pure $ newClient url sesh opts'


getSessionId :: Wreq :> es => String -> Session -> Eff es ByteString
getSessionId url sesh = do
                failGet <- try (get sesh url)
                let sessionId (Right r) = throwIO . TransmissionConnectError $ TransmissionContext "Error why getting X-Session-Id" Nothing Nothing Nothing (Just r) Nothing
                    sessionId (Left e@(HttpExceptionRequest _ (StatusCodeException s _)))
                                  | s ^. (responseStatus . statusCode) == 409 = pure $ s ^. responseHeader sessionIdHeaderName
                                  | otherwise = throwIO e
                    sessionId (Left e) = throwIO e 
                sessionId failGet


-- | Add a torrent to the transfer list
addTorrent :: (FileSystem :> es, Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => TorrentRef -> Timeout -> Maybe FilePath -> Maybe [Int] -> Maybe [Int] -> Maybe Bool -> Maybe Int -> Maybe [Int] -> Maybe [Int] -> Maybe [Int] -> Maybe String -> Maybe [Label] -> Maybe Int -> Eff es (KeyMap Value)
addTorrent tref timeout downloadDir filesUnwanted filesWanted paused peerLimit priorityHigh priorityLow priorityNormal cookies labels bandwidthPriority= do
                                     torrentData <- readTorrent tref
                                     let args = Just . object . (torrentData :) . catMaybes $ [maybeJSON ("download-dir", downloadDir), maybeJSON ("files-unwanted", filesUnwanted), maybeJSON ("files-wanted", filesWanted), maybeJSON ("paused", paused), maybeJSON ("peerLimit", peerLimit), maybeJSON ("priority-high", priorityHigh), maybeJSON ("priority-low", priorityLow), maybeJSON ("priority-normal", priorityNormal), maybeJSON ("cookies", cookies), maybeJSON ("labels", labels), maybeJSON ("bandwidthPriority", bandwidthPriority)]
                                     request TorrentAdd args Nothing False timeout

request :: (HasCallStack, Wreq :> es, Reader Client :> es, Log :> es, Time :> es) => RPCMethod -> Maybe Value -> Maybe [ID] -> Bool -> Timeout -> Eff es (KeyMap Value)
request _ _ Nothing True _ = error "request requires ids"
request _ _ (Just []) True _ = error "request requires ids"
request rpcm args ids reqIDs timeout = do
                                          let args' = fromMaybe Null args
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

examineBody :: Log :> es => Result Value -> RPCMethod -> Value -> Response L.ByteString -> Eff es (KeyMap Value)
examineBody (Fail _ _ e) rpcm query response = logInfo_ (T.pack $ "Error:\n" ++ "Request: " ++ show query ++ "\n" ++ "HTTP Data: " ++  show response) >> throwIO (TransmissionError (TransmissionContext ("failed to parse response as JSON:\n" ++ show e) (Just rpcm ) (Just query) Nothing (Just response) Nothing))
examineBody (Done _ jsonBody@(Object bodyMap)) rpcm query response = case K.lookup (fromString "result") bodyMap of
                                        Nothing -> throwIO . TransmissionError $ TransmissionContext "Query failed, response data missing result:\n" (Just rpcm) (Just query) (Just jsonBody) (Just response) Nothing
                                        Just result -> checkSuccess result
   where
     checkSuccess success
       | success == toJSON "success" =
              case K.lookup (fromString "arguments") bodyMap of
                Just (Object res) -> do
                    case rpcm of
                      TorrentGet        -> pure res
                      TorrentAdd        -> added res
                      SessionGet        -> rawSessionUpdate res
                      SessionStats      ->  undefined
                      PortTest          -> pure res
                      BlocklistUpdate   -> pure res
                      FreeSpace         -> pure res
                      TorrentRenamePath -> pure res
                      _                 -> pure undefined
                _ -> throwIO . TransmissionError $ TransmissionContext "arguments is not an Object" (Just rpcm) (Just query) (Just jsonBody) (Just response) Nothing
      | otherwise = throwIO . TransmissionError $ TransmissionContext ("Query failed with result " ++ show success) (Just rpcm) (Just query) (Just jsonBody) (Just response) Nothing
     added :: KeyMap Value -> Eff es (KeyMap Value)
     added res = do
                   let item
                          | K.member (fromString "torrent-added") res = K.lookup (fromString "torrent-added") res
                          | K.member (fromString "torrent-duplicate") res = K.lookup (fromString "torrent-duplicate") res
                          | otherwise = Nothing
                       result = maybe K.empty (\(Object i) -> K.singleton ((\(A.String s) -> fromText s) . fromJust . K.lookup (fromString "id") $ i) (Object i)) item
                   pure result

examineBody (Done _ r) _ query response = logInfo_ (T.pack $ "Error:\n" ++ "Request: " ++ show query ++ "\n" ++ "HTTP Data: " ++  show response) >> error ("response is not an Object: " ++ show r)

rawSessionUpdate :: a
rawSessionUpdate = undefined
