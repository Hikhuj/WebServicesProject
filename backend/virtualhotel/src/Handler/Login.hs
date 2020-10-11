{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE BlockArguments   #-}
module Handler.Login where

import Import
import Data.Aeson
import Database.Persist.Postgresql
import Data.Aeson.Types
import Data.Maybe (fromJust)

-- Login an User

{- type LoginFields = '[ "usuario", "email", "password" ]
 -
 - data Login = Login {
 -   loginUsuEmail :: Text
 -   , loginUsuPassword :: Text
 - } deriving Show
 -
 - loginForm :: Monad m => FormParser LoginFields Text m Login
 - loginForm =
 -   subParser #usuario (Login
 -     <$> field #email (notEmpty)
 -     <*> field #password notEmpty) -}


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
postUserLoginR :: Handler Value
postUserLoginR = error "NO IMPLEMENTADO"
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
{- encodeUsuario :: UsuarioId -> Usuario -> Handler Value
 - encodeUsuario usuarioId Usuario {..} = do
 -   token <- userIdToToken usuarioId
 -   return $ object
 -     [ "usuario" .= object
 -         [ "usu_email" .= usuarioUsu_email
 -         , "usu_nombre" .= usuarioUsu_nombre
 -         , "token" .= token
 -         ]
 -     ]
 -
 - -- Simple verify Password
 -
 - verifyPass :: Text -> Text -> Bool
 - verifyPass _ _ = False
 - verifyPass loginPassword userPassword =
 -   if loginPassword == userPassword
 -   then True
 -   else False
 - convertToByteString value = BS.pack value -}
