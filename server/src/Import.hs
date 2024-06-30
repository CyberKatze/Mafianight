module Import
(appSalt, maybeAuthId, appJWTSecret)
where
import Yesod.Auth (maybeAuthId)
import Settings (appSalt, appJWTSecret)
