{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Hotel where

import Import
import Database.Persist.Postgresql

getHotelR :: HotelId -> Handler Value
getHotelR hotelId = do
 hotel <- runDB $ get404 hotelId
 return $ object ["hotel" .= hotel]

patchHotelR :: HotelId -> Handler Value
patchHotelR = error "NO IMPLEMENTADO"

putHotelR :: HotelId -> Handler Value
putHotelR hotelId = do
 _ <- runDB $ get404 hotelId
 newHotel <- requireCheckJsonBody :: Handler Hotel
 runDB $ replace hotelId newHotel 
 return $ object ["hotel" .= newHotel]

