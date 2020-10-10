{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Reservation where

import Database.Persist.Postgresql
import Import

getReservationR :: ReservacionId -> Handler Value
getReservationR reservationId = do
 reservation <- runDB $ get404 reservationId
 return $ object ["reservation" .= reservation]

patchReservationR :: ReservacionId -> Handler Value
patchReservationR = error "NO IMPLEMENTADO"

putReservationR :: ReservacionId -> Handler Value
putReservationR reservationId = do
 _ <- runDB $ get404 reservationId
 newReservation <- requireCheckJsonBody :: Handler Reservacion
 runDB $ replace reservationId newReservation 
 return $ object ["reservation" .= newReservation]

