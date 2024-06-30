{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.LoginSpec (spec) where

import TestImport
import Data.Aeson
import Network.Wai.Test      (SResponse(..))
import qualified Web.JWT as J

spec :: Spec
spec = withApp $ do
    describe "Login route" $ do
        it "assert the token returned for a correct email pass" $ do
          secret <- getJWTSecret
          let expectedClaimMap = claimsToClaimsMap Claims {admin = False}
          -- Create a user without admin role
          _ <- createUser "user@example.com" "user" "securepass" False

          let body = object ["email" .= ("user@example.com" :: Text), "password" .= ("securepass" :: Text)]
              encoded = encode body
        
          -- send request with correct email and password
          request $ do
              setMethod "GET"
              setUrl LoginR
              setRequestBody encoded
              addRequestHeader ("Content-Type", "application/json")

          responseBody <- (maybe ("no resonse") simpleBody ) <$> getResponse

          case  ( decode responseBody ):: Maybe Token of
            Just token -> maybe (error "Invalid token") (\ jwt -> assertEq "Claim should be" expectedClaimMap (J.unregisteredClaims (J.claims jwt))) (verifyJWT secret (bearerToken token))
            Nothing -> error "Invalid token"
          statusIs 200
        it "assert error for invalid email or pass" $ do
          -- Create a user without admin role
          _ <- createUser "user@example.com" "user" "securepas" False

          let body = object ["email" .= ("user@example.com" :: Text), "password" .= ("securepass" :: Text)]
              encoded = encode body
        
          -- send request with correct email and password
          request $ do
              setMethod "GET"
              setUrl LoginR
              setRequestBody encoded
              addRequestHeader ("Content-Type", "application/json")

          responseBody <- (maybe ("no resonse") simpleBody ) <$> getResponse

          case  ( decode responseBody ):: Maybe ErrorResp of
            Just _ -> return ()
            Nothing -> error "Invalid error message"

          statusIs 401
