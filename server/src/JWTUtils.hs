{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module JWTUtils where

import Web.JWT
import ClassyPrelude.Yesod
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Map.Strict as M
import qualified Data.Aeson as A 
import Data.Maybe (fromJust)

-- More information for user claims
-- can be data, for more fields 
newtype Claims = Claims
   { admin:: Bool } deriving (Show, Generic)

instance ToJSON Claims
instance FromJSON Claims

claimsToClaimsMap :: Claims -> ClaimsMap 
claimsToClaimsMap c = ClaimsMap $ fromJust ( A.decode (A.encode c ) :: Maybe (M.Map Text Value)) -- type conversion from Recrod to Map

createJWT :: Text -> Text -> Integer -> Claims -> IO Text
createJWT secret user expTime claims = do
  now <- getCurrentTime
  let 
      header = Web.JWT.JOSEHeader { typ = Just "JWT", cty = Nothing, alg = Just HS256, kid = Nothing }
      expTimeUTC = addUTCTime (fromInteger expTime) now
      cs = mempty { 
            Web.JWT.iss = stringOrURI "my-app"
          , Web.JWT.exp = numericDate (utcTimeToPOSIXSeconds expTimeUTC::NominalDiffTime) -- Absolute time of expiration
          , Web.JWT.iat = numericDate (utcTimeToPOSIXSeconds now::NominalDiffTime)
          , Web.JWT.sub = stringOrURI user  -- Add username as the "sub" claim
          ,  unregisteredClaims = claimsToClaimsMap claims -- more claims
          }
          -- add Payload into unregisteredClaims
      key = hmacSecret secret
      token = encodeSigned key header cs 
    in 
      -- TIO.putStrLn $ "Generated JWT: " <> token
      return token

verifyJWT :: Text -> Text -> Maybe (JWT VerifiedJWT)
verifyJWT secret token = decodeAndVerifySignature (toVerify . hmacSecret $  secret) token
