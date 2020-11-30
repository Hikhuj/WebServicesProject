{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Rooms where

import Import
import Database.Persist.Postgresql
import Data.Text

getRoomsR :: Handler Value
getRoomsR = do
 rooms <- runDB $ selectList [HabitacionHab_estado ==. "A"] [Asc HabitacionId]
 sendStatusJSON ok200 rooms

postRoomsR :: Handler Value
postRoomsR = do
 newRoom <- requireCheckJsonBody :: Handler Habitacion
 runDB $ insert newRoom
 sendStatusJSON created201 (object ["New Room" .= newRoom])

