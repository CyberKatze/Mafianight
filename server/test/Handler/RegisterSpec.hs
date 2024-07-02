{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Handler.RegisterSpec (spec) where

import TestImport
import Data.Aeson
import Network.Wai.Test      (SResponse(..))
import qualified Web.JWT as J

spec :: Spec
spec = withApp $ do
  describe "Register route" $ do
    it "assert registration of new user" $ do
      let user = "user"
          email = "user@example.com"
          pass = "securepass"
          user2 = "user2"
          email2 = "user2@gmail.com"
          pass2 = "securepass2"

      _ <- createUser email2 user2 pass2 False

      let body = object ["email" .= ( email::Text ), "userName" .= ( user::Text ), "password" .= ( pass::Text )]
          encoded = encode body

      -- register a new user
      request $ do
          setMethod "POST"
          setUrl RegisterR
          setRequestBody encoded
          addRequestHeader ("Content-Type", "application/json")

      statusIs 200
      
      mtoken <- runMaybeT $ fetchDecodedBody @Token

      case mtoken of
        Nothing -> error "Invalid token"
        Just token -> do

          let body = object ["email" .= (email::Text), "password" .= (pass::Text)]
              encoded = encode body
        
          request $ do
              setMethod "GET"
              setUrl MeR
              addRequestHeader ("Content-Type", "application/json")
              addRequestHeader ("Authorization", "Bearer " <> encodeUtf8(bearerToken token))

          mUserInfo <- runMaybeT $ fetchDecodedBody @UserInfo

          case mUserInfo of
            Nothing -> error "Invalid roles"
            Just userInfo -> do
              assertEq "Should have username " (userInfoUserName userInfo)  user
              assertEq "Should have email " (userInfoEmail userInfo)  email
          statusIs 200

    it "registeration of already registerd username" $ do
      let username = "user"
          email = "user@example2.com"
          pass = "securepass"

      -- Create a user without admin role
      _ <- createUser "user@example.com" username "securepas" False

      let body = object ["email" .= ( email::Text ), "userName" .= ( username::Text ), "password" .= ( pass::Text )]
          encoded = encode body

      -- register a new user
      request $ do
          setMethod "POST"
          setUrl RegisterR
          setRequestBody encoded
          addRequestHeader ("Content-Type", "application/json")

      responseBody <- (maybe ("no resonse") simpleBody ) <$> getResponse

      case  ( decode responseBody ):: Maybe ErrorResp of
        Just _ -> return ()
        Nothing -> error "username is already registers"  

      statusIs 409

    it "registeration of already registerd email" $ do
      let username = "user"
          email = "user@example.com"
          pass = "securepass"

      -- Create a user without admin role
      _ <- createUser email "user2" "securepas" False

      let body = object ["email" .= ( email::Text ), "userName" .= ( username::Text ), "password" .= ( pass::Text )]
          encoded = encode body

      -- register a new user
      request $ do
          setMethod "POST"
          setUrl RegisterR
          setRequestBody encoded
          addRequestHeader ("Content-Type", "application/json")

      responseBody <- (maybe ("no resonse") simpleBody ) <$> getResponse

      case  ( decode responseBody ):: Maybe ErrorResp of
        Just _ -> return ()
        Nothing -> error "email is already registers"  

      statusIs 409
