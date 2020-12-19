{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Hotels where

import Import
import Database.Persist.Postgresql

getHotelsR :: Handler Value
getHotelsR = do
 hotels <- runDB $ selectList [HotelHot_estado ==. "A"] [Asc HotelId]
 sendStatusJSON ok200 hotels

postHotelsR :: Handler Value
postHotelsR = do
 newHotel <- requireCheckJsonBody :: Handler Hotel
 runDB $ insert newHotel
 hotels <- runDB $ selectList [HotelHot_estado ==. "A"] [Asc HotelId]
 sendStatusJSON created201 hotels

