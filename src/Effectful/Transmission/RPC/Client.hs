{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Effectful.Transmission.RPC.Client (
                              Client
                            , runClient
                            , runFullClient
                            , FullClient
                            , getStaticClient
                            , setStaticClient
                            )
where

import           Data.Text                     (Text)
import           Effectful                     (Dispatch (Static), DispatchOf,
                                                Eff, Effect, IOE, (:>))
import           Effectful.Dispatch.Static     (SideEffects (WithSideEffects),
                                                StaticRep, evalStaticRep,
                                                getStaticRep, putStaticRep)
import           Effectful.FileSystem          (FileSystem, runFileSystem)
import           Effectful.Log                 (Log, LogLevel, Logger, runLog)
import           Effectful.Network.HTTP.Client (HttpClient, runHttpClient)
import           Effectful.Time                (Time, runTime)
import           Network.HTTP.Client           (Manager)
import           Transmission.RPC.Session      (emptySession)
import           Transmission.RPC.Types        (ClientRep (ClientRep), URI)

data Client :: Effect

type instance DispatchOf Client = Static WithSideEffects

newtype instance StaticRep Client = Client {unClient :: ClientRep}

type FullClient es = ( Client :> es, HttpClient :> es, Log :> es, Time :> es, FileSystem :> es )

-- constants
currentProtocolVersion :: Int
currentProtocolVersion = 17

-- Effectful methods

getStaticClient :: Client :> es => Eff es ClientRep
getStaticClient = unClient <$> getStaticRep

setStaticClient :: Client :> es => (ClientRep -> ClientRep) -> Eff es ()
setStaticClient f = getStaticClient >>= putStaticRep . Client . f

runClient :: (IOE :> es) => URI -> Eff (Client : es) a -> Eff es a
runClient u = evalStaticRep . Client $ ClientRep u emptySession currentProtocolVersion Nothing Nothing []

runFullClient :: (IOE :> es) => URI -> Manager -> Text -> Logger -> LogLevel -> Eff (Client : HttpClient : Log : Time : FileSystem : es) a -> Eff es a
runFullClient u manager logName logger logLevel = runFileSystem . runTime . runLog logName logger logLevel . runHttpClient manager . runClient u
