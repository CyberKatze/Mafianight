module Handler.Auth where

import Data.Aeson as Aeson

import ClassyPrelude.Yesod
import Foundation
import Model

-- postAuthLoginR :: Handler Value
-- postAuthLoginR = do
--   (username, password) <- requireJsonBody
--
  -- returnJson $ object ["message"::Text .= ("Success" :: Text)]


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
