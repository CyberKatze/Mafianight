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
      user <- createUser email "user" pass False
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

      assertEq "Should have name " (gameName $ entityVal game) name
      assertEq "Should have game Turn " (gameCurrentTurn $ entityVal game) Nothing
      assertEq "Should have userId " (gameUserId $ entityVal  game) (entityKey user)
      statusIs 200
 
    it "asserts game Get request" $ do

      let email = "user@exammple.com"
          pass = "securepass"
          userName = "user"
      -- create a user
      user <- createUser email userName pass False

      let email2 = "user2@exammple.com"
          pass2 = "securepass2"
          userName2 = "user2"
      -- create a user
      user2 <- createUser email2 userName2 pass2 False
      -- login the user
      game <- createGame "sampleGame" (entityKey user)
      _game2 <- createGame "sampleGame2" (entityKey user2)
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
      -- It should only return the game created by the user
      case mResponse of
          Nothing -> error "No response"
          Just response -> assertEq "Should have " (simpleBody response) (encode [game])
                          
      statusIs 200
    it "asserts unauthenticated 403" $ do
      user <- createUser "email.com" "user" "123" False
      _game <- createGame "sampleGame" (entityKey user)
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
    it "asserts get game by id" $ do
      -- create a user
      user <- createUser "email.com" "user" "123" False
      _game <- createGame "sampleGame" (entityKey user)

      mtoken <- login "email.com" "123"

      -- register a game with players
      let name = "game" :: Text
          playersObj =  [ object [ "name" .= ("player1" :: Text) , "alive" .= True, "role" .= object["name" .= ("mafia" :: Text), "id" .= (1::Int64)] ]
                , object [ "name" .= ("player2" :: Text), "alive" .= True, "role" .= object["name" .= ("villager" :: Text), "id" .= (1::Int64)] ]
                ]
          gameObj = object [ "name" .= name, "players" .= playersObj ]
          
          encoded = encode gameObj

      case mtoken of
        Nothing -> error "Invalid token"
        Just (Token {bearerToken = token}) -> do
          request $ do
            setMethod "POST"
            setUrl GameR
            setRequestBody encoded
            addRequestHeader ("Content-Type", "application/json")
            addRequestHeader ("Authorization", "Bearer " <> encodeUtf8(token))

          mGame <- runMaybeT $ fetchDecodedBody @GameInfo

          case mGame of
            Nothing -> error "Invalid game"
            Just (GameInfo {gameInfoId = gameId}) -> do

              request $ do
                setMethod "GET"
                setUrl $ GameIdR gameId
                addRequestHeader ("Content-Type", "application/json")
                addRequestHeader ("Authorization", "Bearer " <> encodeUtf8(token))
              mGameBack <- runMaybeT $ fetchDecodedBody @GameInfo
              -- assert the response
              case mGameBack of
                Nothing -> error "Invalid game"
                Just (GameInfo {gameInfoId = gameIdBack, gameInfoPlayers = players}) -> do
                  assertEq "Should have " gameId gameIdBack
                  -- should exist 2 players
                  assertEq "Should have " 2 (maybe 0 length players)
