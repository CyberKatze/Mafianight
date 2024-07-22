{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Handler.Game where

import ClassyPrelude.Yesod  hiding ((==.), on)
import Foundation
import Model
import  Data.Aeson.Key ()
import Yesod.Auth (maybeAuthId)
import Types ( GameRegister(..), GameInfo(..), RoleInfo(..), PlayerInfo(..))
import Database.Esqueleto.Experimental hiding (Value)

postGameR :: Handler Value
postGameR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).

    maid <- maybeAuthId
    case maid of
      Nothing -> sendStatusJSON status403 $ object [fromString "message" .= ("User must login":: String )]
      Just userId -> do

        game <- (requireCheckJsonBody :: Handler GameRegister)

        insertedGame <- runDB $ insertEntity (Game Nothing (gameRegisterName game)  userId)
        -- If the game has players, insert them into the database
        case gameRegisterPlayers game of
          Just players -> do
            forM_ players $ \player -> do
              -- find roleId of the player


              roles <- case playerInfoRole player of
                Nothing -> return []
                Just r -> do
                  runDB $ select  $ do
                    role <- from $ table @Role
                    where_ ( role ^. RoleName ==. val ( roleInfoName r))
                    pure role

              roleId <- case roles of
                [ Entity roleId _] ->  do
                    return $ Just roleId
                _ -> do
                    return Nothing

              _ <- runDB $ insertEntity (Player {playerRoleId = roleId, playerGameId = entityKey insertedGame,  playerName = playerInfoName player, playerAlive = playerInfoAlive player})
              return ()
          Nothing -> return ()

        returnJson GameInfo { gameInfoName = gameName $ entityVal insertedGame, gameInfoId = fromSqlKey $ entityKey insertedGame, gameInfoPlayers = gameRegisterPlayers game}


getGameR :: Handler Value
getGameR = do
  -- Fetch games from the database
    maid <- maybeAuthId
    case maid of
      Nothing -> sendStatusJSON status403 $ object [fromString "message" .= ("User must login"::String )]
      Just userId -> do
        games <- runDB $ select $ do
          game <- from $ table @Game
          where_ (game ^. GameUserId ==. val userId)
          pure game
      -- Convert games to JSON
        let gamesJson = toJSON games
      -- Return games as JSON response
        returnJson gamesJson

getGameIdR :: Int64 -> Handler Value
getGameIdR queryId = do
  maid <- maybeAuthId
  case maid of
    Nothing -> sendStatusJSON status403 $ object [fromString "message" .= ("User must login"::String )]
    Just userId -> do
      games <- runDB $ select $ do
        game <- from $ table @Game
        where_ (game ^. GameId ==. val (toSqlKey queryId) &&. game ^. GameUserId ==. val userId)
        pure game
      case games of
        [Entity gameId game]-> do
          -- find players of the game
          players <- runDB $ select $ do
            (player :& role) <-
              from $ table @Player
              `leftJoin` table @Role
              `on` (\(player :& role) -> player ^. PlayerRoleId ==.  role ?. RoleId)
            where_ (player ^. PlayerGameId ==. val gameId)
            pure ( player, role )

          let playersInfo = map (\(Entity _ player, mRoleEnt) ->
                                    PlayerInfo { playerInfoName = playerName player
                                                , playerInfoRole = fmap (\entRole ->
                                                  RoleInfo { roleInfoName = roleName $ entityVal entRole, roleInfoDesc = roleDesc $ entityVal entRole, roleInfoAvatar = roleAvatar $ entityVal entRole, roleInfoId = Just $ fromSqlKey $ entityKey entRole }) mRoleEnt
                                                , playerInfoAlive = playerAlive player } )
                                    players

          returnJson GameInfo { gameInfoName = gameName game, gameInfoId = fromSqlKey gameId, gameInfoPlayers = Just playersInfo}
        _ -> sendStatusJSON status404 $ object [fromString "message" .= ("Game not found"::String )]

