{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Rooms where

import Import
import Database.Persist.Postgresql

getRoomsR :: Handler Value
getRoomsR = do
 rooms <- runDB $ selectList [] [Asc HabitacionId]
 sendStatusJSON ok200 (object ["rooms" .= rooms])

postRoomsR :: Handler Value
postRoomsR = do
 newRoom <- requireCheckJsonBody :: Handler Habitacion
 runDB $ insert newRoom
 sendStatusJSON created201 (object ["New Room" .= newRoom])

