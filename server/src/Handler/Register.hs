{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Register where

import Data.Aeson as Aeson
import Data.Aeson.Key ()
import Database.Esqueleto.Experimental hiding (Value)
import ClassyPrelude.Yesod  hiding ((==.), on, (||.))
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
      where_ ( user ^. UserUserName ==. val (userRegisterUserName userRegister) ||. user ^. UserEmail ==. val (userRegisterEmail userRegister))
      pure user
      )

    -- generate token
    case user of
      [Entity _userId _user] -> sendStatusJSON status409 $ object [fromString "message" .= ("username is already registers" )]
      _ -> do
        userId <- runDB $ insert $ User (userRegisterUserName userRegister) pass False (userRegisterEmail userRegister)

        token <-  generateToken userId (Claims { admin = False})
        returnJson Token { bearerToken = token}
              
