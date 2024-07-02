{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types (
    ErrorResp(..)
  , custom403
  , Token(..)
  , Claims(..)
  , UserInfo(..)
  , UserAuth(..)
  , UserRegister(..))
where
import ClassyPrelude.Yesod 
import Data.Aeson.TH
import Data.Aeson
import Data.Char as C (toLower) 

data ErrorResp = ErrorResp
    { message :: Text
    } deriving (Show, Generic)

instance ToJSON ErrorResp
instance FromJSON ErrorResp

custom403 :: MonadHandler m => Text -> m Value
custom403 msg = returnJson $ ErrorResp msg

data Token = Token
  { bearerToken :: Text
  } deriving (Show, Generic)

instance FromJSON Token
instance ToJSON Token


newtype Claims = Claims
   { admin:: Bool } deriving (Show, Generic)

instance ToJSON Claims
instance FromJSON Claims

--  | User Infor for the me route
data UserInfo = UserInfo
  { userInfoEmail :: Text
  , userInfoUserName :: Text
  , userInfoAdmin :: Bool
  } deriving (Show, Generic)

instance ToJSON UserInfo where
  toJSON = genericToJSON $ customOptions "userInfo"
instance FromJSON UserInfo where
  parseJSON = genericParseJSON $ customOptions "userInfo"

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = C.toLower x : xs

customOptions :: String -> Options
customOptions prefix = defaultOptions { fieldLabelModifier = lowerFirst . dropPrefix  prefix}

data UserAuth = UserAuth
  { userAuthEmail :: Text
  , userAuthPassword :: Text
  } deriving (Show, Generic)

instance FromJSON UserAuth where
  parseJSON = genericParseJSON $ customOptions "userAuth"
instance ToJSON UserAuth  where
  toJSON = genericToJSON  $ customOptions "userAuth"

data UserRegister = UserRegister
  { userRegisterEmail :: Text
  , userRegisterUserName :: Text
  , userRegisterPassword :: Text
  } deriving (Show, Generic)

instance FromJSON UserRegister where
  parseJSON = genericParseJSON $ customOptions "userRegister"
instance ToJSON UserRegister  where
  toJSON = genericToJSON  $ customOptions "userRegister"


