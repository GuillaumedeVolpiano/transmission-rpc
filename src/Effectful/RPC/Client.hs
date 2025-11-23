{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Effectful.RPC.Client (
                              Client
                            , runClient
                            , getProtocolVersion
                            , getURI
                            , getClientSession
                            , getServerVersion
                            , getSemVerVersion
                            , setClientSession
                            , setProtocolVersion
                            , setServerVersion
                            , setSemVerVersion
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
                                            serverVersion, session, uri)

data Client :: Effect

type instance DispatchOf Client = Static WithSideEffects

newtype instance StaticRep Client = Client {getClient :: ClientRep}

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

runClient :: (IOE :> es) => URI -> Eff (Client : es) a -> Eff es a
runClient u = evalStaticRep . Client $ ClientRep u emptySession currentProtocolVersion Nothing Nothing
