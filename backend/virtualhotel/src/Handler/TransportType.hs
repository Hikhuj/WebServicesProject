{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.TransportType where

import Database.Persist.Postgresql
import Import

--CRUD for TransportTypes

getTransportTypeR :: TipoTransporteId -> Handler Value
getTransportTypeR transporttypeId = do
 mTransportType <- runDB $ selectFirst [TipoTransporteTip_estado ==. "A", TipoTransporteId ==. transporttypeId ] []
 case mTransportType of
  Just mTransportType -> 
   return $ object ["transporttype" .= mTransportType]
  _ ->
   notFound

--Delete (Under PATCH)
patchTransportTypeR :: TipoTransporteId -> Handler Value
patchTransportTypeR transporttypeId = do
 mTransportTypeId <- runDB $ get transporttypeId
 case mTransportTypeId of
  Just mTransportTypeId ->
   runDB $ update transporttypeId [TipoTransporteTip_estado =. "E"]
  _ -> 
   notFound
 return $ object ["message" .= String "Deleted"]

putTransportTypeR :: TipoTransporteId -> Handler Value
putTransportTypeR transporttypeId = do
 _ <- runDB $ get404 transporttypeId
 newTransportType <- requireCheckJsonBody :: Handler TipoTransporte
 runDB $ replace transporttypeId newTransportType 
 return $ object ["transporttype" .= newTransportType]

