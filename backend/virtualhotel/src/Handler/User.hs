{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.User where

import Database.Persist.Postgresql
import Import

getUserR :: UsuarioId -> Handler Value
getUserR userId = do
 user <- runDB $ get404 userId
 return $ object ["user" .= user]

patchUserR :: UsuarioId -> Handler Value
patchUserR = error "NO IMPLEMENTADO"

putUserR :: UsuarioId -> Handler Value
putUserR userId = do
 _ <- runDB $ get404 userId
 newUser <- requireCheckJsonBody :: Handler Usuario
 runDB $ replace userId newUser 
 return $ object ["user" .= newUser]

