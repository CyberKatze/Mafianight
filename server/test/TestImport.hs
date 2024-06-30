{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Database.Esqueleto.Experimental  (SqlPersistM, runSqlPersistMPool, runSqlPool, rawExecute, rawSql, unSingle, insert, Entity, insertEntity, entityKey)
import qualified Database.Esqueleto.Experimental  as X
import Database.Persist.SqlBackend (getEscapedRawName)
import Foundation            as X 
import Model                 as X
import Test.Hspec            as X
import Text.Shakespeare.Text (st)
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X hiding (LoginR)
import Yesod.Test            as X
import JWTUtils              as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)
import Settings              (appSalt, appJWTSecret)
import Yesod.Auth.Util.PasswordStore (makePasswordSalt, makeSalt)
import Network.Wai.Test      as X (SResponse(..))
import Data.Aeson
import Control.Monad.Trans.Maybe as X
import qualified Data.ByteString.Lazy as BL

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler


withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    -- TRUNCATEing all tables is the simplest approach to wiping the database.
    -- Should your application grow to hundreds of tables and tests,
    -- switching to DELETE could be a substantial speedup.
    -- See: https://github.com/yesodweb/yesod-scaffold/issues/201
    let escapedTables = map (\t -> getEscapedRawName t sqlBackend) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: DB [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_type = 'BASE TABLE';
    |] []

    return $ map unSingle tables

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
-- authenticateAs :: Entity User -> YesodExample App ()
-- authenticateAs (Entity _ u) = do
--     request $ do
--         setMethod "POST"
--         addPostParam "ident" $ userIdent u
--         setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.  The dummy email entry helps to confirm that foreign-key
-- checking is switched off in wipeDB for those database backends which need it.
hashPasswordTest :: Text -> YesodExample App Text
hashPasswordTest password = do
  salt <- encodeUtf8 . appSalt . appSettings <$> getTestYesod
  return $ decodeUtf8 $ makePasswordSalt (encodeUtf8 password) (makeSalt salt) 16

getJWTSecret :: YesodExample App Text
getJWTSecret = appJWTSecret . appSettings <$> getTestYesod

generateTokenTest :: Text -> Claims -> YesodExample App Text
generateTokenTest user claims = do
  secret <-  appJWTSecret . appSettings <$> getTestYesod
  token <- liftIO $ createJWT secret user 3600 claims
  return token

createUser :: Text -> Text -> Text -> Bool -> YesodExample App (Entity User)
createUser email userName pass admin = do 
    pass <- hashPasswordTest pass
    runDB $ do    
      user <- insertEntity User
          { userUserName = userName
          , userPassword = Just pass
          , userAdmin = admin
          }
      _ <- insert Email
          { emailEmail = email
          , emailUserId = entityKey user
          , emailVerkey = Nothing
          }
      return user

login :: Text -> Text  -> YesodExample App (Maybe Token)
login email pass = do
  let body = object ["email" .= email, "password" .= pass]
      encoded = encode body

  -- send request with correct email and password
  request $ do
      setMethod "GET"
      setUrl LoginR
      setRequestBody encoded
      addRequestHeader ("Content-Type", "application/json")
  
  (maybe Nothing ((maybe Nothing id) . (decode) . simpleBody) ) <$> getResponse

createGame :: Text -> YesodExample App (Entity Game)
createGame name = runDB $ do
    game <- insertEntity Game
        { gameName = name
        , gameCurrentTurn = Nothing
        }
    return game

-- Generalized function using MaybeT to fetch and decode body
fetchDecodedBody :: (FromJSON a) =>  MaybeT (YesodExample App) a
fetchDecodedBody  = do
    mresponse <- lift getResponse
    case mresponse of
      Nothing -> error "No response"
      Just response -> do
        case decode (simpleBody response) of
            Just decodedValue -> return decodedValue
            Nothing           -> error "Failed to decode response body"
