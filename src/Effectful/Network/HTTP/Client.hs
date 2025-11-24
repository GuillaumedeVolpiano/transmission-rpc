{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Effectful.Network.HTTP.Client (
                                       HttpClient
                                     , runHttpClient
                                     , getManager
                                     , httpLbs
                                     )
where
import Effectful (Effect, DispatchOf, Dispatch(Static), Eff, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects(WithSideEffects), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import Network.HTTP.Client (Manager, Response, Request)
import Data.ByteString.Lazy (ByteString)
import qualified Network.HTTP.Client as HC (httpLbs)

data HttpClient :: Effect

type instance DispatchOf HttpClient = Static WithSideEffects

newtype instance StaticRep HttpClient = HttpClient {
                                                  manager :: Manager
                                                }

runHttpClient :: IOE :> es => Manager -> Eff (HttpClient : es) a -> Eff es a
runHttpClient m = evalStaticRep $ HttpClient m

getManager :: HttpClient :> es => Eff es Manager
getManager = manager <$> getStaticRep


httpLbs :: HttpClient :> es => Request -> Eff es (Response ByteString)
httpLbs req = do
  m <- getManager 
  unsafeEff_ $ HC.httpLbs req m
