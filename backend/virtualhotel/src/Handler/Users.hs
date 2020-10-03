{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.Users where

import Import
import Database.Persist.Postgresql

getUsersR :: Handler Value
getUsersR = do
 users <- runDB $ selectList [] [Asc UsuarioId]
 sendStatusJSON ok200 (object ["users" .= users])

postUsersR :: Handler Value
postUsersR = do
 newUser <- requireCheckJsonBody :: Handler Usuario
 runDB $ insert newUser
 sendStatusJSON created201 (object ["New User" .= newUser])

