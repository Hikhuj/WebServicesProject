{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Transports where

import Import
import Database.Persist.Postgresql
-- CRUD for Transports
getTransportsR :: Handler Value
getTransportsR = do
 transports <- runDB $ selectList [TransporteTra_estado ==. "A"] [Asc TransporteId]
 sendStatusJSON ok200 (object ["transports" .= transports])

postTransportsR :: Handler Value
postTransportsR = do
 newTransport <- requireCheckJsonBody :: Handler Transporte
 _ <- runDB $ insert newTransport
 sendStatusJSON created201 (object ["New Transport" .= newTransport])

