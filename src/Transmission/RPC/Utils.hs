{-# LANGUAGE FlexibleContexts #-}

module Transmission.RPC.Utils
  (
  getTorrentArguments)

where

import qualified Data.HashMap.Strict        as M (filter, keys)
import           Transmission.RPC.Constants (torrentGetArgs)
import           Transmission.RPC.Types     (Args (Args))

getTorrentArguments :: Int -> [String]
getTorrentArguments protocolVersion = M.keys . M.filter version $ torrentGetArgs
  where
    version (Args _ v rv _ _ _) = protocolVersion >= v && maybe True (> protocolVersion) rv
