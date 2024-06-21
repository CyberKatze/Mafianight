module Handler.User where

import Data.Aeson as Aeson
import ClassyPrelude.Yesod   
import Foundation
import Model

postUserR :: Handler Value
postUserR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    turn <- (requireCheckJsonBody :: Handler User)

    insertedUser <- runDB $ insertEntity turn
    returnJson insertedUser


getUserR :: Handler Value
getUserR = do
    -- Fetch users from the database
    users <- runDB $ selectList [] [Desc UserId]
    -- Convert users to JSON
    let usersJson = Aeson.toJSON users
    -- Return users as JSON response
    returnJson usersJson

