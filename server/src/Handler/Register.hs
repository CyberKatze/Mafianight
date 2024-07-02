{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Register where

import Data.Aeson as Aeson
import Data.Aeson.Key ()
import Database.Esqueleto.Experimental hiding (Value)
import ClassyPrelude.Yesod  hiding ((==.), on)
import Foundation
import Model
import Types ( Token(..), UserRegister(..), Claims(..))


postRegisterR :: Handler Value
postRegisterR = do

    userRegister <- (requireCheckJsonBody :: Handler UserRegister)
    pass <- hashPassword (userRegisterPassword userRegister)

    -- check if the userName is already registered
    user <- runDB(  
      select $ do
      user <- 
        from $ table @User
      where_ ( user ^. UserUserName ==. val (userRegisterUserName userRegister))
      pure user
      )
    -- check if the email is already registered
    email <- runDB(  
      select $ do
      email <- 
        from $ table @Email
      where_ ( email ^. EmailEmail ==. val (userRegisterEmail userRegister))
      pure email
      )

    -- generate token
    case user of
      [Entity userId user] -> sendStatusJSON status409 $ object [fromString "message" .= ("username is already registers" )]
      _ -> do
        case email of
          [Entity emailId email] -> sendStatusJSON status409 $ object [fromString "message" .= ("email is already registers" )]
          _ -> do
            
            userId <- runDB $ insert $ User (userRegisterUserName userRegister) pass False
            _ <- runDB $ insert $ Email (userRegisterEmail userRegister) userId Nothing

            token <-  generateToken userId (Claims { admin = False})
            returnJson Token { bearerToken = token}
              
