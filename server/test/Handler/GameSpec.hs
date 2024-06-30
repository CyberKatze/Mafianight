{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Handler.GameSpec (spec) where

import TestImport
import Data.Aeson
import Database.Esqueleto.Experimental

spec :: Spec
spec = withApp $ do
  describe "Game page" $ do
    it "insert a game with post and get it back" $ do
      let name = "game" :: Text
          body = object [ "name" .= name ]
          encoded = encode body

      let email = "user@exammple.com"
          pass = "securepass"
      -- create a user
      _ <- createUser email "user" pass False
      -- login the user
 
      mtoken <- login email pass
      case mtoken of
        Nothing -> error "Invalid token"
        Just (Token {bearerToken = token}) -> do
          request $ do
            setMethod "POST"
            setUrl GameR
            setRequestBody encoded
            addRequestHeader ("Content-Type", "application/json")
            addRequestHeader ("Authorization", "Bearer " <> encodeUtf8(token))
 
      statusIs 200
      games <- runDB$ select $ from $ table @Game
      game <-  case games of
        [ent] -> pure ent
        _ -> error "needed 1 entity"
      assertEq "Should have " (gameName $ entityVal game) name
      statusIs 200
 
    it "asserts game Get request" $ do
      game <- createGame "sampleGame"

      let email = "user@exammple.com"
          pass = "securepass"
      -- create a user
      _ <- createUser email "user" pass False
      -- login the user
      mtoken <- login email pass

      case mtoken of
        Nothing -> error "Invalid token"
        Just (Token {bearerToken = token}) -> do
          request $ do
             setMethod "GET"
             setUrl GameR
             addRequestHeader ("Content-Type", "application/json")
             addRequestHeader ("Authorization", "Bearer " <> encodeUtf8(token))

      mResponse <- getResponse
      case mResponse of
          Nothing -> error "No response"
          Just response -> assertEq "Should have " (simpleBody response) (encode [game])
                          
      statusIs 200
    it "asserts unauthenticated 403" $ do
        _ <- createGame "sampleGame"
        -- sent a get request
        request $ do
            setMethod "GET"
            setUrl GameR
            addRequestHeader ("Content-Type", "application/json")
 
        mResponse <- getResponse
        case mResponse of
            Nothing -> error "No response"
            Just response -> do
              case decode (simpleBody response) of
                Just (ErrorResp {message = msg}) -> assertEq "Should have " msg "User not authenticated"
                _ -> error "Invalid response"
        statusIs 403
