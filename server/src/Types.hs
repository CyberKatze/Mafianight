{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Types (ErrorResp(..), custom403, Token(..), Claims(..))
where
import ClassyPrelude.Yesod 

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
