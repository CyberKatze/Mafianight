{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.MeSpec (spec) where

import TestImport
import Data.Aeson

spec :: Spec
spec = withApp $ do
    describe "Test me route" $ do
        it "extract user info from the token" $ do
            let email1 = "user@exammple.com"
                pass1 = "securepass"
                username1 = "user"
                username2 = "user2"
                email2 = "user@exampe2.com"
                pass2 = "securepass2"
                            

            -- create a user
            _ <- createUser email1 username1 pass1 False
            _ <- createUser email2 username2 pass2 False

            -- load seed data
            mtoken <- login email1 pass1

            case mtoken of
              Nothing -> error "Invalid token"
              Just token -> do

                request $ do
                    setMethod "GET"
                    setUrl MeR
                    addRequestHeader ("Content-Type", "application/json")
                    addRequestHeader ("Authorization", "Bearer " <> encodeUtf8(bearerToken token))

                -- decode the response body to Roles
                mUserInfo <- runMaybeT fetchDecodedBody:: YesodExample App (Maybe UserInfo)

                case mUserInfo of
                  Nothing -> error "Invalid roles"
                  Just userInfo -> do
                    assertEq "Should have " (userInfoUserName userInfo)  username1
                    assertEq "Should have " (userInfoEmail userInfo)  email1

                statusIs 200

        it "asserts unauthenticated 403" $ do
            let email1 = "user@exammple.com"
                pass1 = "securepass"
                username1 = "user"
                username2 = "user2"
                email2 = "user@exampe2.com"
                pass2 = "securepass2"
                            

            -- create a user
            _ <- createUser email1 username1 pass1 False
            _ <- createUser email2 username2 pass2 False
            -- sent a get request
            request $ do
                setMethod "GET"
                setUrl MeR
                addRequestHeader ("Content-Type", "application/json")
     
            mResponse <- getResponse
            case mResponse of
                Nothing -> error "No response"
                Just response -> do
                  case decode (simpleBody response) of
                    Just (ErrorResp {message = msg}) -> assertEq "Should have " msg "User not authenticated"
                    _ -> error "Invalid response"
            statusIs 403
