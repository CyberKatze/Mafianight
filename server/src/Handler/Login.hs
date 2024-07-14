{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
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

    users <- runDB(  
      select $ do
      user <- 
        from $ table @User
      where_ ( user ^. UserEmail ==. val(userAuthEmail userAuth) &&. user ^. UserPassword ==. val pass)
      pure user
      )

    -- generate token
    case users of
      [Entity userId user] -> do
        token <-  generateToken userId (Claims { admin = userAdmin user})
        returnJson Token { bearerToken = token}
              
      _ -> sendStatusJSON status401 $ object [fromString "message" .= ("Invalid username or password" )]


