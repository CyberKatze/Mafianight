module Handler.Turn where

import Data.Aeson as Aeson
import ClassyPrelude.Yesod   
import Foundation
import Model

postTurnR :: Handler Value
postTurnR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    turn <- (requireCheckJsonBody :: Handler Turn)

    insertedTurn <- runDB $ insertEntity turn
    returnJson insertedTurn


getTurnR :: Handler Value
getTurnR = do
    -- Fetch turns from the database
    turns <- runDB $ selectList [] [Desc TurnId]
    -- Convert turns to JSON
    let turnsJson = Aeson.toJSON turns
    -- Return turns as JSON response
    returnJson turnsJson
