{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Register where

import Import
import Database.Persist.Postgresql



{- Register new usuario -}


postUserRegisterR :: Handler Value
postUserRegisterR = do
 newUsuario <- requireCheckJsonBody :: Handler Usuario
 usuarioId <- runDB $ insert newUsuario
 encodeUsuario usuarioId newUsuario

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

