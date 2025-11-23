{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Effectful.Network.HTTP.Client (
                                       HttpClient
                                     , runHttpClient
                                     , getManager
                                     , httpLbs
                                     , setHeaders
                                     )
where
import Effectful (Effect, DispatchOf, Dispatch(Static), Eff, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects(WithSideEffects), StaticRep, evalStaticRep, getStaticRep, unsafeEff_, putStaticRep)
import Network.HTTP.Client (Manager, Response, Request, requestHeaders)
import Data.ByteString.Lazy (ByteString)
import qualified Network.HTTP.Client as HC (httpLbs)
import Network.HTTP.Types.Header (Header)

data HttpClient :: Effect

type instance DispatchOf HttpClient = Static WithSideEffects

data instance StaticRep HttpClient = HttpClient {
                                                  manager :: Manager
                                                , headers :: [Header]
                                                }

runHttpClient :: IOE :> es => Manager -> Eff (HttpClient : es) a -> Eff es a
runHttpClient m = evalStaticRep $ HttpClient m []

getManager :: HttpClient :> es => Eff es Manager
getManager = manager <$> getStaticRep

getHeaders :: HttpClient :> es => Eff es [Header]
getHeaders = headers <$> getStaticRep

setHeaders :: HttpClient :> es => [Header] -> Eff es ()
setHeaders h = getStaticRep >>= \c -> putStaticRep (c{headers = h})

httpLbs :: HttpClient :> es => Request -> Eff es (Response ByteString)
httpLbs req = do
  m <- getManager 
  h <- getHeaders
  let fullReq = req {requestHeaders = requestHeaders req ++ h}
  unsafeEff_ $ HC.httpLbs fullReq m
