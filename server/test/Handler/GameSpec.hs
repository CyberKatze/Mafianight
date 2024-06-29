{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.GameSpec (spec) where

import TestImport
import Data.Aeson
import Network.Wai.Test      (SResponse(..))
import Settings (appJWTSecret)

spec :: Spec
spec = withApp $ do
    describe "Game page" $ do
        it "insert a game with post and get it back" $ do
            setting <- appSettings <$> getTestYesod
            let name = "game" :: Text
                body = object [ "name" .= name ]
                encoded = encode body

            token <- liftIO $ createJWT (appJWTSecret setting) "admin" 1000  Claims {admin = True}

            request $ do
                setMethod "POST"
                setUrl GameR
                setRequestBody encoded
                addRequestHeader ("Content-Type", "application/json")
                addRequestHeader ("Authorization", "Bearer " <> encodeUtf8(token))

            statusIs 200
            games <- runDB $ selectList [GameName ==. name] []
            Entity _id game <-
                case games of
                    [ent] -> pure ent
                    _ -> error "needed 1 entity"
            assertEq "Should have " game (Game Nothing name)

        it "asserts game Get request" $ do
            game <- createGame "sampleGame"
            token <- liftIO $ createJWT "secret" "admin" 1000  Claims {admin = True}
            -- sent a get request
            request $ do
                setMethod "GET"
                setUrl GameR
                addRequestHeader ("Content-Type", "application/json")
                addRequestHeader ("Authorization", "Bearer " <> encodeUtf8(token))

            mResponse <- getResponse
            case mResponse of
                Nothing -> error "No response"
                Just response -> assertEq "Should have " (simpleBody response) (encode [game])
                            
            -- Check the json
            statusIs 200
