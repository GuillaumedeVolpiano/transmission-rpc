{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Transmission.RPC.MonadTransmission.Generic () where
import Transmission.RPC.MonadTransmission(MonadClient, MonadHttp, getClient, setClient, httpLbs, MonadTransmission, throwIO, try, hGetContents, readFile)
import Transmission.RPC.Types (ClientRep)
import Data.IORef (IORef, readIORef, modifyIORef')
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Time (MonadTime)
import Log (MonadLog)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HC (httpLbs)
import Control.Monad.IO.Unlift (withRunInIO, MonadUnliftIO)
import qualified Control.Exception as E (throwIO, try)
import qualified Data.ByteString as B (hGetContents, readFile)

instance (Monad m, MonadIO m, MonadReader (IORef ClientRep) m, MonadLog m, MonadTime m) => MonadClient m where
  getClient = ask >>= liftIO . readIORef
  setClient s = ask >>= \r -> liftIO . modifyIORef' r $ s

instance (Monad m, MonadReader Manager m, MonadIO m) => MonadHttp m where
  httpLbs req = ask >>= liftIO . HC.httpLbs req

instance (Applicative m, Monad m, MonadIO m, MonadUnliftIO m, MonadClient m, MonadHttp m) => MonadTransmission m where
  throwIO = liftIO . E.throwIO
  try ma = withRunInIO $ \run -> E.try (run ma)
  hGetContents = liftIO . B.hGetContents
  readFile = liftIO . B.readFile
