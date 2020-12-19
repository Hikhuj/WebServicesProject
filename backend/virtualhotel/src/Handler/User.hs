{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.User where

import Database.Persist.Postgresql
import Import

getUserR :: UsuarioId -> Handler Value
getUserR userId = do
 mUser <- runDB $ selectFirst [UsuarioUsu_estado ==. "A", UsuarioId ==. userId ] []
 case mUser of
  Just mUser -> 
   returnJson mUser
  _ ->
   notFound

--Delete (Under PATCH)
patchUserR :: UsuarioId -> Handler Value
patchUserR userId = do
 mUserId <- runDB $ get userId
 case mUserId of
  Just mUserId ->
   runDB $ update userId [UsuarioUsu_estado =. "E"]
  _ -> 
   notFound
 users <- runDB $ selectList [UsuarioUsu_estado ==. "A"] []
 sendStatusJSON ok200 users

putUserR :: UsuarioId -> Handler Value
putUserR userId = do
 _ <- runDB $ get404 userId
 newUser <- requireCheckJsonBody :: Handler Usuario
 runDB $ replace userId newUser 
 returnJson newUser

