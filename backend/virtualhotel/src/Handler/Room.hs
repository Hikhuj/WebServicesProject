{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Room where

import Database.Persist.Postgresql
import Import

getRoomR :: HabitacionId -> Handler Value
getRoomR roomId = do
 room <- runDB $ get404 roomId
 return $ object ["room" .= room]

patchRoomR :: HabitacionId -> Handler Value
patchRoomR = error "NO IMPLEMENTADO"

putRoomR :: HabitacionId -> Handler Value
putRoomR roomId = do
 _ <- runDB $ get404 roomId
 newRoom <- requireCheckJsonBody :: Handler Habitacion
 runDB $ replace roomId newRoom 
 return $ object ["room" .= newRoom]

