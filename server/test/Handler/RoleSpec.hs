{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.RoleSpec (spec) where

import TestImport
import Seeder (loadSeedData)

spec :: Spec
spec = withApp $ do
    describe "Test role route" $ do
        it "give us list of roles with status 200" $ do
            let email = "user@exammple.com"
                pass = "securepass"

            -- create a user
            _ <- createUser email "user" pass False

            -- load seed data
            connPool <-  appConnPool  <$> getTestYesod
            liftIO $ runSqlPool loadSeedData connPool

            mtoken <- login email pass
            case mtoken of
              Nothing -> error "Invalid token"
              Just token -> do

                request $ do
                    setMethod "GET"
                    setUrl RoleR
                    addRequestHeader ("Content-Type", "application/json")
                    addRequestHeader ("Authorization", "Bearer " <> encodeUtf8(bearerToken token))

                -- decode the response body to Roles
                mRoles <- runMaybeT fetchDecodedBody:: YesodExample App (Maybe [Role])

                case mRoles of
                  Nothing -> error "Invalid roles"
                  Just roles -> do
                    -- should have a roles with name "mafia" in the list
                    assertNotEq "Should have " (filter (((==) "mafia"). toLower . roleName) roles) []
                    --should have a roles with name "villager" in the list
                    assertNotEq "Should have " (filter (((==) "villager"). toLower . roleName) roles) []
                                    

                statusIs 200
