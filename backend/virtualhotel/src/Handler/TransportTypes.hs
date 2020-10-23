{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.TransportTypes where

import Import
import Database.Persist.Postgresql

getTransportTypesR :: Handler Value
getTransportTypesR = do
 users <- runDB $ selectList [TipoTransporteTip_estado ==. "A"] [Asc TipoTransporteId]
 sendStatusJSON ok200 (object ["transport types" .= users])

postTransportTypesR :: Handler Value
postTransportTypesR = do
 newTransportType <- requireCheckJsonBody :: Handler TipoTransporte
 _ <- runDB $ insert newTransportType
 sendStatusJSON created201 (object ["New TransportType" .= newTransportType])

