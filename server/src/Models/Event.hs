{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.Event where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson
import Prelude

data EventType = Killed | Voted | Saved
     deriving (Show, Read, Eq, Generic)
derivePersistField "EventType"

instance ToJSON EventType
instance FromJSON EventType
