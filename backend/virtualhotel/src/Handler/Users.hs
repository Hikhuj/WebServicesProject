{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Users where

import Import
import Database.Persist.Postgresql

--CRUD for Users

getUsersR :: Handler Value
getUsersR = do
 users <- runDB $ selectList [UsuarioUsu_estado ==. "A"] [Asc UsuarioId]
 sendStatusJSON ok200 (object ["users" .= users])

-- Deprecated. See Login Handler
postUsersR :: Handler Value
postUsersR = do
 newUser <- requireCheckJsonBody :: Handler Usuario
 _ <- runDB $ insert newUser
 sendStatusJSON created201 (object ["New User" .= newUser])

