{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
module Handler.Login where

import Data.Aeson as Aeson
import Data.Aeson.Key ()
import Database.Esqueleto.Experimental hiding (Value)
import ClassyPrelude.Yesod  hiding ((==.), on)
import Foundation
import Model
import Types ( Token(..), UserAuth(..), Claims(..))

postLoginR :: Handler Value
postLoginR = do

    userAuth <- (requireCheckJsonBody :: Handler UserAuth)
    pass <- hashPassword (userAuthPassword userAuth)

    -- Find the user by email
    maybeUser <- runDB $ 
      selectOne $ do
        (user :& email) <- 
          from $ table @User
          `innerJoin` table @Email
          `on` (\(user :& email) -> user ^. UserId ==. email ^. EmailUserId)
        where_ (email ^. EmailEmail ==. val (userAuthEmail userAuth))
        pure user

    case maybeUser of
      Just (Entity userId user) -> do
        -- Verify the password by direct comparison
        if pass== userPassword user
          then do
            -- Generate token
            token <- generateToken userId (Claims { admin = userAdmin user})
            returnJson Token { bearerToken = token }
          else do
            sendStatusJSON status401 $ object [fromString "message" .= "Invalid email or password" ]
      Nothing -> do
        sendStatusJSON status401 $ object [fromString "message" .= "Invalid email or password" ]

