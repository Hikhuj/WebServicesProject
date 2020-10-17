{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DeriveGeneric   #-}
module Handler.Login where

import Import
import Database.Persist.Postgresql

data Login = Login {
  email :: Text
  , pass :: Text
  
} deriving (Show, Generic)

instance FromJSON Login 
instance ToJSON Login 

postUserLoginR :: Handler Value
postUserLoginR = do
 login <- requireCheckJsonBody :: Handler Login
 let userMail = email login
 let userPass = pass login
 mUser <- runDB $ selectFirst [UsuarioUsu_email ==. userMail ] []
 case mUser of
   Just (Entity usuarioId usuario@Usuario {..}) | validPwd ->
     encodeUsuario usuarioId usuario
     where validPwd = verifyPass userPass usuarioUsu_password
   _ ->
     notAuthenticated


{- postUserLoginR :: Handler Value
 - postUserLoginR =
 -  withForm loginForm $ \Login {..} -> do
 -    mUser <- runDB $get404 (toSqlKey UsuarioUsu_email) loginUsuEmail
 -    case mUser of
 -      Just (Entity usuarioId usuario@Usuario {..}) | validPwd ->
 -        encodeUsuario usuarioId usuario
 -        where validPwd = verifyPass loginPassword usuarioUsu_password
 -      _ ->
 -        notAuthenticated -}

{- data LoginData = LoginData {
 -     email :: Text
 -   , password :: Text
 - } -}
{- 
 - instance FromJSON LoginData
 - instance ToJSON LoginData
 -  -}
  {- newLogin <- requireCheckJsonBody :: Maybe LoginData
   - let contents = convertToByteString newLogin
   - let Just mEmail = decode newLogin :: Maybe Text
   - return $ object (["message" .= mEmail]) -}
  {- case mUser of
   -   Just (Entity usuarioId usuario@Usuario {..}) | validPwd ->
   -     encodeUsuario usuarioId usuario
   -     where validPwd = verifyPass mPassword usuarioUsu_password
   -   _ ->
   -     notAuthenticated -}

-- | Encode a 'Usuario' with a JWT authentication token.
encodeUsuario :: UsuarioId -> Usuario -> Handler Value
encodeUsuario usuarioId Usuario {..} = do
  token <- userIdToToken usuarioId
  return $ object
    [ "usuario" .= object
     [ "usu_email" .= usuarioUsu_email
      , "usu_nombre" .= usuarioUsu_nombre
      , "token" .= token
      ]
    ]

-- Simple verify Password

verifyPass :: Text -> Text -> Bool
verifyPass loginPassword userPassword =
  loginPassword == userPassword
