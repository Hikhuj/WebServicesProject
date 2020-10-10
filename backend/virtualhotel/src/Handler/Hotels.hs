{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Hotels where

import Import
import Database.Persist.Postgresql

getHotelsR :: Handler Value
getHotelsR = do
 hotels <- runDB $ selectList [] [Asc HotelId]
 sendStatusJSON ok200 (object ["hotels" .= hotels])

postHotelsR :: Handler Value
postHotelsR = do
 newHotel <- requireCheckJsonBody :: Handler Hotel
 runDB $ insert newHotel
 sendStatusJSON created201 (object ["New Hotel" .= newHotel])
