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
import Database.Esqueleto.Experimental (ConnectionPool, runSqlPool)
import Settings 
import ClassyPrelude.Yesod 

import Data.Kind            (Type)
import Control.Monad.Logger (LogSource)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Auth hiding (LoginR)
import Model (UserId)
import Types ( custom403)
import Utils
import Web.JWT (stringOrURIToText, claims, sub)
import Yesod.Auth.Util.PasswordStore (makePasswordSalt, makeSalt)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

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
    errorHandler NotAuthenticated = toTypedContent <$> custom403 "User not authenticated"
    errorHandler (PermissionDenied msg) = toTypedContent <$> custom403 msg
    errorHandler NotFound = toTypedContent <$> custom403 "Resource not found"
    errorHandler (InvalidArgs _) = toTypedContent <$> custom403 "Invalid arguments"
    errorHandler (BadMethod _) = toTypedContent <$> custom403 "Invalid method"
    errorHandler (InternalError e) = toTypedContent <$> custom403 e
    errorHandler other = defaultErrorHandler other

    isAuthorized RoleR _ = return Authorized
    isAuthorized GameR _ = isUser
    isAuthorized UserR _ = isUser
    isAuthorized TurnR _ = isUser
    isAuthorized EventR _ = isUser
    isAuthorized PlayerR _ = isUser
    isAuthorized MeR _ = isUser
    isAuthorized LoginR _ = return Authorized
    isAuthorized RegisterR _ = return Authorized


instance YesodAuth App where
  type AuthId App = UserId

  loginDest _ = LoginR
  logoutDest _ = LoginR

  -- maybeAuthId  :: (MonadHandler m, App ~ HandlerSite m )  => m (Maybe (Auth App))
  maybeAuthId =  do
    mtoken <- lookupBearerAuth
    secret <-  (appJWTSecret . appSettings) <$> getYesod
    case mtoken of
      Just token -> do
        case verifyJWT secret token of
          Just c -> do 
            return $ textToKey . stringOrURIToText <$> (sub $ claims c)
          Nothing -> return Nothing
      Nothing -> return Nothing


instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

isUser :: HandlerFor App AuthResult
isUser = do
  maid <- maybeAuthId

  case maid of
    Just _ -> return Authorized
    Nothing -> return $ Unauthorized "User not authenticated"


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

hashPassword :: Text -> HandlerFor App Text
hashPassword password = do
  salt <- encodeUtf8 . appSalt . appSettings <$> getYesod
  return $ decodeUtf8 $ makePasswordSalt (encodeUtf8 password) (makeSalt salt) 16

generateToken :: UserId -> Claims -> HandlerFor App Text
generateToken userId claim = do
  secret <-  appJWTSecret . appSettings <$> getYesod
  token <- liftIO $ createJWT secret userId 3600 claim
  return token
