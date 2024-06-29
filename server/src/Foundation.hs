{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
module Foundation where

import Yesod.Core.Types     (Logger)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Settings 
import ClassyPrelude.Yesod 

import Data.Kind            (Type)
import Control.Monad.Logger (LogSource)
import qualified Yesod.Core.Unsafe as Unsafe
import JWTUtils

-- Used only when in "auth-dummy-login" setting is enabled.

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }
-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

data ErrorResp = ErrorResp
    { message :: Text
    } deriving (Show, Generic)

instance ToJSON ErrorResp

custom403 :: Text -> Handler Value
custom403 msg = returnJson $ ErrorResp msg
-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger
    errorHandler NotAuthenticated = toTypedContent <$> custom403 "You must login to access this resource"
    errorHandler (PermissionDenied msg) = toTypedContent <$> custom403 msg
    errorHandler NotFound = toTypedContent <$> custom403 "Resource not found"
    errorHandler other = defaultErrorHandler other

    isAuthorized RoleR _ = return Authorized
    isAuthorized GameR _ = isUser
    isAuthorized UserR _ = isUser
    isAuthorized TurnR _ = isUser
    isAuthorized EventR _ = isUser
    isAuthorized PlayerR _ = isUser

-- lookupBearerAuth :: Handler (Maybe Text)
-- lookupBearerAuth = do
--   token <- lookupHeader "Authorization"
--   case token of
--     Just bs -> if "Bearer " `isPrefixOf` bs
--               then return $ Just $ drop 7 $ decodeUtf8 bs
--               else return Nothing
--     Nothing -> return Nothing

isUser :: HandlerFor App AuthResult
isUser = do
  mtoken <- lookupBearerAuth
  setting <- appSettings <$> getYesod
  case mtoken of
    Just token -> do
      let secret = appJWTSecret setting
      case verifyJWT secret token of
        Just _ -> return Authorized
        Nothing -> return $ Unauthorized "Invalid token"
    Nothing -> return $ Unauthorized "You must login to access this resource"


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
