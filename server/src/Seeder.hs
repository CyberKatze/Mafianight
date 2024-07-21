{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExplicitForAll #-}
module Seeder (loadSeedData) where

import ClassyPrelude.Yesod
import Model
import Data.Aeson (decodeStrict)

-- Define a function to load seed data from a directory
loadSeedData ::  ReaderT SqlBackend IO ()
loadSeedData  = do
    -- Get a list of all files in the seeder directory
    let rolePath = "seeds/roles.json"
    -- Read the file content (assuming JSON format for this example)
    content <- liftIO $ readFileUtf8 rolePath
    -- Parse the content and insert into the database
    -- Here you would add your logic to parse and insert data
    -- Example for inserting roles from a JSON file
    case decodeStrict (encodeUtf8 content):: (Maybe [Role]) of
        Just roles ->  do
          -- upsert each role
          forM_ roles $ \r -> do
              _ <- upsert r [RoleName =. roleName r, RoleDesc =. roleDesc r, RoleAvatar =. roleAvatar r, RoleMafia =. roleMafia r]
              return ()
        Nothing -> liftIO $ putStrLn $ "Failed to parse " ++ pack rolePath

