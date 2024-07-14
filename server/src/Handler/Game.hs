{-# LANGUAGE TypeApplications #-}
module Handler.Game where

import ClassyPrelude.Yesod  hiding ((==.), on)
import Foundation
import Model
import  Data.Aeson.Key ()
import Yesod.Auth (maybeAuthId)
import Types ( GameRegister(..), GameInfo(..))
import Database.Esqueleto.Experimental hiding (Value)

postGameR :: Handler Value
postGameR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
          
    maid <- maybeAuthId
    case maid of
      Nothing -> sendStatusJSON status403 $ object [fromString "message" .= ("User must login" )]
      Just userId -> do
      
        game <- (requireCheckJsonBody :: Handler GameRegister)

        insertedGame <- runDB $ insertEntity (Game Nothing (gameRegisterName game)  userId)
        returnJson GameInfo { gameInfoName = gameName $ entityVal insertedGame, gameInfoId = fromSqlKey $ entityKey insertedGame }


getGameR :: Handler Value
getGameR = do
  -- Fetch games from the database
    maid <- maybeAuthId
    case maid of
      Nothing -> sendStatusJSON status403 $ object [fromString "message" .= ("User must login" )]
      Just userId -> do
        games <- runDB $ select $ do 
          game <- from $ table @Game
          where_ (game ^. GameUserId ==. val userId)
          pure game
      -- Convert games to JSON
        let gamesJson = toJSON games
      -- Return games as JSON response
        returnJson gamesJson

