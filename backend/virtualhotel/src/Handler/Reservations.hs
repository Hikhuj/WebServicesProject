{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Reservations where

import Import
import Database.Persist.Postgresql

getReservationsR :: Handler Value
getReservationsR = do
 reservations <- runDB $ selectList [ReservacionRes_estado ==. "A"] [Asc ReservacionId]
 sendStatusJSON ok200 reservations

postReservationsR :: Handler Value
postReservationsR = do
 newReservation <- requireCheckJsonBody :: Handler Reservacion
 runDB $ insert newReservation
 reservations <- runDB $ selectList [ReservacionRes_estado ==. "A"] [Asc ReservacionId]
 sendStatusJSON created201 reservations

