{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module AuthSpec (spec) where

import TestImport
import Web.JWT as J
import Data.Maybe (fromJust)

spec :: Spec
spec =  do
    describe "create a jwt and verify it" $ do
      it "creates a jwt" $ do
        token <- createJWT "secret" "admin" 1000  Claims {admin = True}
        case (verifyJWT "secret" token) of
          Nothing -> error "No token"
          Just a -> shouldBe (J.claims a) (J.claims . fromJust .  J.decode $ token)

