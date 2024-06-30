module Handler.Game where

import ClassyPrelude.Yesod   
import Foundation
import Model
import  Data.Aeson.Key ()

postGameR :: Handler Value
postGameR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
          
    game <- (requireCheckJsonBody :: Handler Game)

    insertedGame <- runDB $ insertEntity game
    returnJson insertedGame


getGameR :: Handler Value
getGameR = do
  -- Fetch games from the database
    games <- runDB $ selectList [] [Desc GameId]
  -- Convert games to JSON
    let gamesJson = toJSON games
  -- Return games as JSON response
    returnJson gamesJson

