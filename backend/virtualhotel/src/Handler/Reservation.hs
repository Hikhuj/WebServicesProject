{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Reservation where

import Database.Persist.Postgresql
import Import

--GET
getReservationR :: ReservacionId -> Handler Value
getReservationR reservationId = do
 mReservation <- runDB $ selectFirst [ReservacionRes_estado ==. "A", ReservacionId ==. reservationId ] []
 case mReservation of
  Just mReservation -> 
   returnJson mReservation
  _ ->
   notFound

--Delete (Under PATCH)
patchReservationR :: ReservacionId -> Handler Value
patchReservationR reservationId = do
 mReservationId <- runDB $ get reservationId
 case mReservationId of
  Just mReservationId ->
   runDB $ update reservationId [ReservacionRes_estado =. "E"]
  _ -> 
   notFound
 return $ object ["message" .= String "Deleted"]

-- UPDATE / REPLAC
putReservationR :: ReservacionId -> Handler Value
putReservationR reservationId = do
 _ <- runDB $ get404 reservationId
 newReservation <- requireCheckJsonBody :: Handler Reservacion
 runDB $ replace reservationId newReservation 
 return $ object ["reservation" .= newReservation]

