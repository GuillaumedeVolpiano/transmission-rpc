module Transmission.RPC.Constants 
  (defaultTimeout, sessionIdHeaderName)
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.CaseInsensitive (CI, mk)

defaultTimeout :: Int
defaultTimeout = 3000

sessionIdHeaderName :: CI ByteString
sessionIdHeaderName = mk . pack $ "X-Transmission-Session-Id"


