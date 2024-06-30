{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Utils ( Claims(..), claimsToClaimsMap, textToKey, keyToText, createJWT, verifyJWT) where
import ClassyPrelude.Yesod 
import Database.Esqueleto.Experimental (toSqlKey, fromSqlKey)
import Web.JWT
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Map.Strict as M
import qualified Data.Aeson as A 
import Model (UserId)
import Types 
import Text.Read (read)
import Data.Maybe (fromJust)
import qualified Data.Text as T (unpack) 
  
keyToText :: UserId -> Text
keyToText = pack . show . fromSqlKey

textToKey :: Text -> UserId
textToKey = toSqlKey . (read) . T.unpack


claimsToClaimsMap :: Claims -> ClaimsMap 
claimsToClaimsMap c = ClaimsMap $ fromJust ( A.decode (A.encode c ) :: Maybe (M.Map Text Value)) -- type conversion from Recrod to Map

createJWT :: Text -> UserId -> Integer -> Claims -> IO Text
createJWT secret userId expTime claim = do
  now <- getCurrentTime
  let 
      header = Web.JWT.JOSEHeader { typ = Just "JWT", cty = Nothing, alg = Just HS256, kid = Nothing }
      expTimeUTC = addUTCTime (fromInteger expTime) now
      cs = mempty { 
            Web.JWT.iss = stringOrURI "my-app"
          , Web.JWT.exp = numericDate (utcTimeToPOSIXSeconds expTimeUTC::NominalDiffTime) -- Absolute time of expiration
          , Web.JWT.iat = numericDate (utcTimeToPOSIXSeconds now::NominalDiffTime)
          , Web.JWT.sub = stringOrURI (keyToText userId)  -- Add username as the "sub" claim
          ,  unregisteredClaims = claimsToClaimsMap claim -- more claims
          }
          -- add Payload into unregisteredClaims
      key = hmacSecret secret
      token = encodeSigned key header cs 
    in 
      -- TIO.putStrLn $ "Generated JWT: " <> token
      return token

verifyJWT :: Text -> Text -> Maybe (JWT VerifiedJWT)
verifyJWT secret token = decodeAndVerifySignature (toVerify . hmacSecret $  secret) token
