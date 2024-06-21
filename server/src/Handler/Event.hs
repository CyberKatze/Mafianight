module Handler.Event where

import Data.Aeson as Aeson
import ClassyPrelude.Yesod   
import Foundation
import Model

postEventR :: Handler Value
postEventR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    turn <- (requireCheckJsonBody :: Handler Event)

    insertedEvent <- runDB $ insertEntity turn
    returnJson insertedEvent


getEventR :: Handler Value
getEventR = do
    -- Fetch events from the database
    events <- runDB $ selectList [] [Desc EventId]
    -- Convert events to JSON
    let eventsJson = Aeson.toJSON events
    -- Return events as JSON response
    returnJson eventsJson
