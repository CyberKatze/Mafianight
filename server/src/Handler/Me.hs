{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Me where

import Data.Aeson as Aeson
import Data.Aeson.Key ()
import Database.Esqueleto.Experimental hiding (Value)
import ClassyPrelude.Yesod  hiding ((==.), on)
import Foundation
import Model
import Types (Claims(..), Token(..), UserInfo(..))
import Yesod.Auth (maybeAuthId)


getMeR :: Handler Value
getMeR = do

    maid <- maybeAuthId
    case maid of
      Nothing -> sendStatusJSON status403 $ object [fromString "message" .= ("User must login" )]
      Just userId -> do
        -- user <- runDB $ selectFirst [UserUserName ==. (username userAuth), UserPassword ==. Just pass] []
        userEmail <- runDB(  
          select $ do
          (user :& email) <- 
            from $ table @User
            `innerJoin` table @Email
            `on` (\(user :& email) -> user ^. UserId ==. email ^. EmailUserId)
          where_ ( user ^. UserId ==. val userId)
          pure (user, email)
          )

        -- -- generate token
        case userEmail of
          [(Entity _ user, Entity _ email)] -> do
            returnJson UserInfo { email = emailEmail email, userName = userUserName user, isAdmin = userAdmin user}
                  
                  
          _ -> sendStatusJSON status401 $ object [fromString "message" .= ("Invalid username or password" )]

