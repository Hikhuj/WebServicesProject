{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Room where

import Database.Persist.Postgresql
import Import

--CRUD for Rooms

--Get
getRoomR :: HabitacionId -> Handler Value
getRoomR roomId = do
 mRoom <- runDB $ selectFirst [HabitacionHab_estado ==. "A", HabitacionId ==. roomId ] []
 case mRoom of
  Just mRoom -> 
   returnJson mRoom
  _ ->
   notFound

--Delete (Under PATCH)
patchRoomR :: HabitacionId -> Handler Value
patchRoomR roomId = do
 mRoomId <- runDB $ get roomId
 case mRoomId of
  Just mRoomId ->
   runDB $ update roomId [HabitacionHab_estado =. "E"]
  _ -> 
   notFound
 rooms <- runDB $ selectList [HabitacionHab_estado ==. "A"] [Asc HabitacionId]
 sendStatusJSON ok200 rooms

--Update
putRoomR :: HabitacionId -> Handler Value
putRoomR roomId = do
 _ <- runDB $ get404 roomId
 newRoom <- requireCheckJsonBody :: Handler Habitacion
 runDB $ replace roomId newRoom 
 return $ object ["room" .= newRoom]

