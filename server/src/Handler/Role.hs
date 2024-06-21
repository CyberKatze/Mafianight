module Handler.Role where

import Data.Aeson as Aeson

import ClassyPrelude.Yesod   
import Foundation
import Model

postRoleR :: Handler Value
postRoleR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    role <- (requireCheckJsonBody :: Handler Role)

    insertedRole <- runDB $ insertEntity role
    returnJson insertedRole


getRoleR :: Handler Value
getRoleR = do
  -- Fetch roles from the database
    roles <- runDB $ selectList [] [Desc RoleId]
  -- Convert roles to JSON
    let rolesJson = Aeson.toJSON roles
  -- Return roles as JSON response
    returnJson rolesJson

getRoleNameR :: Text -> Handler Value
getRoleNameR _name = do
  -- Fetch roles from the database
    roles <- runDB $ selectList [] [Desc RoleId]
  -- Convert roles to JSON
    let rolesJson = Aeson.toJSON roles
  -- Return roles as JSON response
    returnJson rolesJson
