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
import Types ( Token(..))
import Types (Claims(..))

data UserAuth = UserAuth
  { email :: Text
  , password :: Text
  } deriving (Show, Generic)

instance FromJSON UserAuth
instance ToJSON UserAuth

getLoginR :: Handler Value
getLoginR = do

    userAuth <- (requireCheckJsonBody :: Handler UserAuth)
    pass <- hashPassword (password userAuth)

    -- user <- runDB $ selectFirst [UserUserName ==. (username userAuth), UserPassword ==. Just pass] []
    user <- runDB(  
      select $ do
      (user :& _email) <- 
        from $ table @User
        `leftJoin` table @Email
        `on` (\(user :& _email) -> just (user ^. UserId) ==. _email ?. EmailUserId)
      where_ ( user ^. UserPassword ==. val (Just pass))
      pure user
      )

    -- generate token
    case user of
      [Entity userId user] -> do
        token <-  generateToken userId (Claims { admin = userAdmin user})
        returnJson Token { bearerToken = token}
              
      _ -> sendStatusJSON status401 $ object [fromString "message" .= ("Invalid username or password" )]


