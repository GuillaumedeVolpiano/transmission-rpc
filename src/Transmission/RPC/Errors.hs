module Transmission.RPC.Errors (
      TransmissionError(..)
    , TransmissionContext (..)
                               )

where

import           Data.Aeson.Types       (Value)
import qualified Data.ByteString.Lazy   as L (ByteString)
import           Effectful.Exception    (Exception)
import           Network.HTTP.Client    (Response, Request)
import           Transmission.RPC.Types (RPCMethod)

data TransmissionError = TransmissionAuthError TransmissionContext
                       | TransmissionConnectError TransmissionContext
                       | TransmissionTimeoutError TransmissionContext
                       | TransmissionError TransmissionContext
                       deriving Show

data TransmissionContext = TransmissionContext {
                                             message :: String
                                           , usedMethod :: Maybe RPCMethod
                                           , argument :: Maybe Value
                                           , jsonResponse :: Maybe Value
                                           , rawResponse :: Maybe (Response L.ByteString)
                                           , sentRequest :: Maybe Request
                                           } deriving Show



instance Exception TransmissionError

