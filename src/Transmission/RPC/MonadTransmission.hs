{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Transmission.RPC.MonadTransmission   (
                                              MonadTransmission(..)
                                            , MonadHttp(..)
                                            , MonadClient(..)
                                            )

where
import           Control.Exception                  (Exception)
import           Control.Monad.Time                 (MonadTime)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as L (ByteString)
import           Data.Text                          (Text)
import           Effectful                          (Eff, (:>))
import qualified Effectful.Exception                as EE (throwIO, try)
import           Effectful.FileSystem               (FileSystem)
import qualified Effectful.FileSystem.IO.ByteString as EF (hGetContents,
                                                           readFile)
import           Effectful.Log                      (Log)
import           Effectful.Network.HTTP.Client      (HttpClient)
import qualified Effectful.Network.HTTP.Client      as EH (httpLbs)
import           Effectful.Time                     (Time)
import           Effectful.Transmission.RPC.Client  (Client)
import qualified Effectful.Transmission.RPC.Client  as EC (getClientSession,
                                                           getProtocolVersion,
                                                           getSemVerVersion,
                                                           getServerVersion,
                                                           getURI,
                                                           setClientSession,
                                                           setProtocolVersion,
                                                           setSemVerVersion,
                                                           setServerVersion,
                                                           getHeaders,
                                                           setHeaders)
import           GHC.Stack                          (HasCallStack)
import           Log                                (MonadLog)
import           Network.HTTP.Client                (Request, Response)
import           Network.HTTP.Types                 (Header)
import           System.IO                          (Handle)
import           Transmission.RPC.Session           (Session)
import           Transmission.RPC.Types             (URI)


class (Applicative m, Monad m, MonadClient m, MonadHttp m) => MonadTransmission m where
  throwIO :: (HasCallStack, Exception e) => e -> m a
  try :: Exception e => m a -> m (Either e a)
  hGetContents :: Handle -> m ByteString
  readFile :: FilePath -> m ByteString

class Monad m => MonadHttp m where
  httpLbs :: Request -> m (Response L.ByteString)

class (Monad m, MonadLog m, MonadTime m) => MonadClient m where
  getProtocolVersion :: m Int
  getURI :: m URI
  getClientSession :: m Session
  setClientSession :: Session -> m ()
  getServerVersion :: m (Maybe Text)
  getSemVerVersion :: m (Maybe Text)
  setProtocolVersion :: Int -> m ()
  setServerVersion :: Maybe Text -> m ()
  setSemVerVersion :: Maybe Text -> m ()
  setHeaders :: [Header] -> m ()
  getHeaders :: m [Header]

instance (FileSystem :> es, HttpClient :> es, Client :> es, Log :> es, Time :> es)
  => MonadTransmission (Eff es) where
    throwIO = EE.throwIO
    try = EE.try
    hGetContents = EF.hGetContents
    readFile = EF.readFile

instance (HttpClient :> es) => MonadHttp (Eff es) where
    httpLbs = EH.httpLbs

instance (Client :> es, Log :> es, Time :> es) => MonadClient (Eff es) where
    getProtocolVersion = EC.getProtocolVersion
    getURI = EC.getURI
    getClientSession = EC.getClientSession
    setClientSession = EC.setClientSession
    getServerVersion = EC.getServerVersion
    getSemVerVersion = EC.getSemVerVersion
    setProtocolVersion = EC.setProtocolVersion
    setServerVersion = EC.setServerVersion
    setSemVerVersion = EC.setSemVerVersion
    setHeaders = EC.setHeaders
    getHeaders = EC.getHeaders
