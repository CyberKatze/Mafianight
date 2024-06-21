{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Models.Turn where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson
import Prelude

data Phase = Day | Night | Ended
     deriving (Show, Read, Eq, Generic)
derivePersistField "Phase"

instance ToJSON Phase
instance FromJSON Phase
