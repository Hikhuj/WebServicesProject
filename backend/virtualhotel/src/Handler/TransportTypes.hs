{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.TransportTypes where

import Import
import Database.Persist.Postgresql

getTransportTypesR :: Handler Value
getTransportTypesR = do
 transporttypes <- runDB $ selectList [TipoTransporteTip_estado ==. "A"] [Asc TipoTransporteId]
 sendStatusJSON ok200 transporttypes

postTransportTypesR :: Handler Value
postTransportTypesR = do
 newTransportType <- requireCheckJsonBody :: Handler TipoTransporte
 _ <- runDB $ insert newTransportType
 sendStatusJSON created201 newTransportType

