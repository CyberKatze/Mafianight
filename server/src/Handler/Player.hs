module Handler.Player where

import Data.Aeson as Aeson
import ClassyPrelude.Yesod   
import Foundation
import Model

postPlayerR :: Handler Value
postPlayerR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    player <- (requireCheckJsonBody :: Handler Player)

    insertedPlayer <- runDB $ insertEntity player
    returnJson insertedPlayer


getPlayerR :: Handler Value
getPlayerR = do
  -- Fetch players from the database
    players <- runDB $ selectList [] [Desc PlayerId]
  -- Convert players to JSON
    let playersJson = Aeson.toJSON players
  -- Return players as JSON response
    returnJson playersJson
