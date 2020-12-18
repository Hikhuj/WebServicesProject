{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Transport where

import Database.Persist.Postgresql
import Import

--CRUD for Transport
getTransportR :: TransporteId -> Handler Value
getTransportR transportId = do
 mTransport <- runDB $ selectFirst [TransporteTra_estado ==. "A", TransporteId ==. transportId ] []
 case mTransport of
  Just mTransport -> 
   returnJson mTransport
  _ ->
   notFound

--Delete (Under PATCH)
patchTransportR :: TransporteId -> Handler Value
patchTransportR transportId = do
 mTransportId <- runDB $ get transportId
 case mTransportId of
  Just mTransportId ->
   runDB $ update transportId [TransporteTra_estado =. "E"]
  _ -> 
   notFound
 return $ object ["message" .= String "Deleted"]

putTransportR :: TransporteId -> Handler Value
putTransportR transportId = do
 _ <- runDB $ get404 transportId
 newTransport <- requireCheckJsonBody :: Handler Transporte
 runDB $ replace transportId newTransport 
 return $ object ["transport" .= newTransport]

