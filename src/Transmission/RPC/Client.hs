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
import qualified Data.Aeson as A (Value(String))
import           Data.Aeson.Key                  (fromString, fromText)
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap               as K (lookup, member, empty, singleton)
import           Data.Aeson.Parser               (json)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import           Data.ByteString.Lazy            (ByteString)
import           Data.Fixed                      (E3, Fixed, showFixed)
import           Data.Maybe                      (catMaybes, fromMaybe, fromJust)
import qualified Data.Text                       as T (pack)
import           Effectful                       (Eff, (:>))
import           Effectful.FileSystem            (FileSystem)
import           Effectful.Log                   (Log, logAttention_, logInfo_)
import           Effectful.Reader.Static         (Reader, asks)
import           Effectful.Time                  (Time, monotonicTime)
import           Effectful.Wreq                  (Options, Response, Wreq,
                                                  manager, responseBody)
import           Effectful.Wreq.Session          (Session, postWith)
import           Network.HTTP.Client             (defaultManagerSettings,
                                                  managerResponseTimeout,
                                                  responseTimeoutMicro)
import           Transmission.RPC.Types          (Client, ID, Label, RPCMethod(..), 
                                                  Timeout, TorrentRef, getOpts,
                                                  getSession, getURI, newClient)
import           Transmission.RPC.Utils          (maybeJSON, readTorrent)

-- | Create a client from a URL, a timeout and a Logger
fromUrl :: String -> Session -> Options -> Transmission.RPC.Types.Client
fromUrl = Transmission.RPC.Types.newClient

-- | Add a torrent to the transfer list
addTorrent :: (FileSystem :> es, Wreq :> es, Reader Transmission.RPC.Types.Client :> es, Log :> es, Time :> es) => Transmission.RPC.Types.TorrentRef -> Transmission.RPC.Types.Timeout -> Maybe FilePath -> Maybe [Int] -> Maybe [Int] -> Maybe Bool -> Maybe Int -> Maybe [Int] -> Maybe [Int] -> Maybe [Int] -> Maybe String -> Maybe [Transmission.RPC.Types.Label] -> Maybe Int -> Eff es (KeyMap Value)
addTorrent tref timeout downloadDir filesUnwanted filesWanted paused peerLimit priorityHigh priorityLow priorityNormal cookies labels bandwidthPriority= do
                                     torrentData <- readTorrent tref
                                     let args = Just . object . (torrentData :) . catMaybes $ [maybeJSON ("download-dir", downloadDir), maybeJSON ("files-unwanted", filesUnwanted), maybeJSON ("files-wanted", filesWanted), maybeJSON ("paused", paused), maybeJSON ("peerLimit", peerLimit), maybeJSON ("priority-high", priorityHigh), maybeJSON ("priority-low", priorityLow), maybeJSON ("priority-normal", priorityNormal), maybeJSON ("cookies", cookies), maybeJSON ("labels", labels), maybeJSON ("bandwidthPriority", bandwidthPriority)]
                                     request Transmission.RPC.Types.TorrentAdd args Nothing False timeout

request :: (Wreq :> es, Reader Transmission.RPC.Types.Client :> es, Log :> es, Time :> es) => Transmission.RPC.Types.RPCMethod -> Maybe Value -> Maybe [Transmission.RPC.Types.ID] -> Bool -> Transmission.RPC.Types.Timeout -> Eff es (KeyMap Value)
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

examineBody :: Log :> es => Result Value -> Transmission.RPC.Types.RPCMethod -> Value -> Response ByteString -> Eff es (KeyMap Value)
examineBody (Fail _ _ e) _ query response = logInfo_ (T.pack $ "Error:\n" ++ "Request: " ++ show query ++ "\n" ++ "HTTP Data: " ++  show response) >> error ("failed to parse response as JSON: " ++ e)
examineBody (Done _ (Object bodyMap)) rpcm _ _ = case K.lookup (fromString "result") bodyMap of
                                        Nothing -> error ("Query failed, response data missing result:\n" ++ show bodyMap)
                                        Just result -> checkSuccess result
   where
     checkSuccess success
       | success == toJSON "success" =
              case K.lookup (fromString "arguments") bodyMap of
                Just (Object res) -> do
                    case rpcm of
                      Transmission.RPC.Types.TorrentGet -> pure res
                      Transmission.RPC.Types.TorrentAdd -> added res
                      Transmission.RPC.Types.SessionGet -> rawSessionUpdate res
                      Transmission.RPC.Types.SessionStats ->  undefined
                      Transmission.RPC.Types.PortTest -> pure res
                      Transmission.RPC.Types.BlocklistUpdate -> pure res
                      Transmission.RPC.Types.FreeSpace -> pure res
                      Transmission.RPC.Types.TorrentRenamePath -> pure res
                      _ -> pure undefined
                _ -> error "arguments is not an Object"
      | otherwise = error ("Query failed with result " ++ show success)
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
