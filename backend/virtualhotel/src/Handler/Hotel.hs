{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Hotel where

import Import
import Database.Persist.Postgresql

getHotelR :: HotelId -> Handler Value
getHotelR hotelId = do
 mHotel <- runDB $ selectFirst [HotelHot_estado ==. "A", HotelId ==. hotelId ] []
 case mHotel of
  Just mHotel -> 
   returnJson mHotel
  _ ->
   notFound

--Delete (Under PATCH)
patchHotelR :: HotelId -> Handler Value
patchHotelR hotelId = do
 mHotelId <- runDB $ get hotelId
 case mHotelId of
  Just mHotelId ->
   runDB $ update hotelId [HotelHot_estado =. "E"]
  _ -> 
   notFound
 return $ object ["message" .= String "Deleted"]

putHotelR :: HotelId -> Handler Value
putHotelR hotelId = do
 _ <- runDB $ get404 hotelId
 newHotel <- requireCheckJsonBody :: Handler Hotel
 runDB $ replace hotelId newHotel 
 returnJson newHotel

