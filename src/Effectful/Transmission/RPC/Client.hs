{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ConstraintKinds #-}

module Effectful.Transmission.RPC.Client (
                              Client
                            , runClient
                            , runFullClient
                            , getProtocolVersion
                            , getURI
                            , getClientSession
                            , getServerVersion
                            , getSemVerVersion
                            , setClientSession
                            , setProtocolVersion
                            , setServerVersion
                            , setSemVerVersion
                            , getHeaders
                            , setHeaders
                            , FullClient
                            )
where

import           Data.Text                 (Text)
import           Effectful                 (Dispatch (Static), DispatchOf, Eff,
                                            Effect, IOE, (:>))
import           Effectful.Dispatch.Static (SideEffects (WithSideEffects),
                                            StaticRep, evalStaticRep,
                                            getStaticRep, putStaticRep)
import           Lens.Micro                ((.~), (^.))
import qualified Transmission.RPC.Session  as TS (Session)
import           Transmission.RPC.Session  (emptySession)
import           Transmission.RPC.Types    (ClientRep (ClientRep), URI,
                                            protocolVersion, semVerVersion,
                                            serverVersion, session, uri, headers)
import Network.HTTP.Client (Manager)
import Effectful.Log (Logger, Log, runLog, LogLevel)
import Effectful.Network.HTTP.Client (HttpClient, runHttpClient)
import Effectful.Time (Time, runTime)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Network.HTTP.Types (Header)

data Client :: Effect

type instance DispatchOf Client = Static WithSideEffects

newtype instance StaticRep Client = Client {getClient :: ClientRep}

type FullClient es = ( Client :> es, HttpClient :> es, Log :> es, Time :> es, FileSystem :> es )

-- constants
currentProtocolVersion :: Int
currentProtocolVersion = 17

-- Effectful methods

getStaticClient :: Client :> es => Eff es ClientRep
getStaticClient = getClient <$> getStaticRep

setStaticClient :: Client :> es => (ClientRep -> ClientRep) -> Eff es ()
setStaticClient f = getStaticClient >>= putStaticRep . Client . f

getProtocolVersion :: Client :> es => Eff es Int
getProtocolVersion = (^. protocolVersion) <$> getStaticClient

setProtocolVersion :: Client :> es => Int -> Eff es ()
setProtocolVersion pv = setStaticClient (protocolVersion .~ pv)

getURI :: Client :> es => Eff es String
getURI = (^. uri) <$> getStaticClient

getClientSession :: Client :> es => Eff es TS.Session
getClientSession = (^. session) <$> getStaticClient

setClientSession :: Client :> es => TS.Session -> Eff es ()
setClientSession sesh = setStaticClient (session .~ sesh)

getServerVersion :: Client :> es => Eff es (Maybe Text)
getServerVersion = (^. serverVersion) <$> getStaticClient

setServerVersion :: Client :> es => Maybe Text -> Eff es ()
setServerVersion sv = setStaticClient (serverVersion .~ sv)

getSemVerVersion :: Client :> es => Eff es (Maybe Text)
getSemVerVersion = (^. semVerVersion) <$> getStaticClient

setSemVerVersion :: Client :> es => Maybe Text -> Eff es ()
setSemVerVersion svv = setStaticClient (semVerVersion .~ svv)

getHeaders :: Client :> es => Eff es [Header]
getHeaders = (^. headers) <$> getStaticClient

setHeaders :: Client :> es => [Header] -> Eff es ()
setHeaders h = setStaticClient (headers .~ h)

runClient :: (IOE :> es) => URI -> Eff (Client : es) a -> Eff es a
runClient u = evalStaticRep . Client $ ClientRep u emptySession currentProtocolVersion Nothing Nothing []

runFullClient :: (IOE :> es) => URI -> Manager -> Text -> Logger -> LogLevel -> Eff (Client : HttpClient : Log : Time : FileSystem : es) a -> Eff es a 
runFullClient u manager logName logger logLevel = runFileSystem . runTime . runLog logName logger logLevel . runHttpClient manager . runClient u
