{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.GithubProfile where
import ClassyPrelude.Yesod 
import Foundation 
import Network.HTTP.Simple (httpLBS, parseRequest, setRequestHeader, getResponseBody, getResponseStatusCode, getResponseStatus)  
import Data.Aeson (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as L8

import Settings 

-- Define the data type to represent a GitHub profile  
data GitHubProfile = GitHubProfile  
    { login :: Text         -- GitHub username  
    , avatar_url :: Text    -- Avatar image URL  
    , html_url :: Text      -- Profile URL  
    , bio :: Maybe Text     -- Optional bio  
    } deriving (Show, Generic)  

instance FromJSON GitHubProfile  
instance ToJSON GitHubProfile


-- Handler to fetch GitHub profile using provided username  
getGithubProfileR :: Text -> Handler Value  
getGithubProfileR username = do  
    token  <- getGithubToken 
    let requestUrl = "https://api.github.com/users/" ++ username  
    initReq <- parseRequest $ unpack requestUrl  
    let request = setRequestHeader "Authorization" ["token " <> token]  
                $ setRequestHeader "Accept" ["application/vnd.github.v3+json"]  
                $ setRequestHeader "User-Agent" ["YourAppName/1.0"]  -- Add User-Agent header  
                $ initReq  
    
    response <- liftIO $ httpLBS request  
    let statusCodeReq = getResponseStatusCode response  
    let responseBodyReq = getResponseBody response  

    -- Log the response for diagnostic purposes  
    liftIO $ L8.putStrLn  responseBodyReq

    case statusCodeReq of  
        200 -> case decode responseBodyReq:: Maybe GitHubProfile of  
            Just profile -> returnJson profile  
            Nothing -> invalidArgs ["Invalid JSON response from GitHub API"]  
        403 -> permissionDenied "GitHub API returned '403 Forbidden'. Check your token permissions."  
        _ -> sendResponseStatus (toEnum statusCodeReq) $ object ["error" .= ("Unexpected response from GitHub API" :: Text)]
