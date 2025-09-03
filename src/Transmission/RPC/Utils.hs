{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeOperators    #-}
module Transmission.RPC.Utils
  (
  maybeJSON
  , readTorrent
  , getTorrentArguments)

where

import           Data.Aeson                         (ToJSON, toJSON)
import           Data.Aeson.Key                     (fromString)
import           Data.Aeson.Types                   (Pair)
import           Data.ByteString.UTF8               (toString)
import           Effectful                          (Eff, (:>))
import           Effectful.FileSystem               (FileSystem)
import           Effectful.FileSystem.IO.ByteString (hGetContents)
import qualified Effectful.FileSystem.IO.ByteString as FS (readFile)
import           Transmission.RPC.Types             (TorrentRef (..), Args(Args))
import Transmission.RPC.Constants (torrentGetArgs)
import qualified Data.HashMap.Strict as M (keys, filter)

maybeJSON :: ToJSON a => (String, Maybe a) -> Maybe Pair
maybeJSON (s, a) = (fromString s, ) . toJSON <$> a

readTorrent :: FileSystem :> es => TorrentRef -> Eff es Pair
readTorrent (TorrentURI u)     = pure (fromString "filename", toJSON u)
readTorrent (Path fp)          = (fromString "metainfo",) . toJSON . toString <$> FS.readFile fp
readTorrent (TorrentContent b) = pure . (fromString "metainfo", ) . toJSON . toString $ b
readTorrent (Binary h)         = (fromString "metainfo", ) . toJSON . toString <$> hGetContents h

getTorrentArguments :: Int -> [String]
getTorrentArguments protocolVersion = M.keys . M.filter version $ torrentGetArgs  
  where
    version (Args _ v rv _ _ _) = protocolVersion >= v && maybe True (> protocolVersion) rv
