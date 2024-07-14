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
import Types ( UserInfo(..))
import Yesod.Auth (maybeAuthId)


getMeR :: Handler Value
getMeR = do

    maid <- maybeAuthId
    case maid of
      Nothing -> sendStatusJSON status403 $ object [fromString "message" .= ("User must login" )]
      Just userId -> do
        users <- runDB(  
          select $ do
          (user) <- 
            from $ table @User
          where_ ( user ^. UserId ==. val userId)
          pure (user)
          )

        -- -- generate token
        case users of
          [Entity _ user] -> do
            returnJson UserInfo { userInfoEmail = userEmail user, userInfoUserName = userUserName user, userInfoAdmin = userAdmin user}
                  
                  
          _ -> sendStatusJSON status401 $ object [fromString "message" .= ("Invalid username or password" )]

