{-# LANGUAGE OverloadedStrings #-}
module Routes.Routes where

import Data.Text (Text, unpack)
import Data.Text.Lazy (toStrict, fromStrict)
-- import Web.Scotty
import Common.Views (layout, loginView, bootstrap3Link, LoginForm(..), LoginErrors(..))

import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Blaze.ByteString.Builder (Builder)

import Text.Digestive.Scotty
import Validators (validateLogin)
import Text.Digestive.View (absoluteRef, View)
import Text.Digestive (errors)
import Text.Digestive.Form (validate, (.:), Form)

import Avtor (loggedInUserId, LoggedInUser, SignInDto, 
              SignUpDto, 
              SignUpDto(..), 
              User(User), 
              User(..),
              signIn, 
              SignInDeps(..), 
              SignInDto(..), 
              SignInError(SignInErrorIpRestricted, SignInErrorLoginAttemptsExceeded, SignInErrorUserNotFound, SignInErrorIncorrectPassword),
              UserId(..))

import Web.Scotty.Trans
import Web.Scotty.Internal.Types (ScottyError)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.IORef (IORef)
import Common.Commons (idxOr)

import qualified Data.Text           as Text
import qualified Data.List           as List
import qualified Web.Scotty          as Scotty
import qualified Text.Digestive.Form as Form
import qualified Repo.Sql.UserRepo as UserRepo
import qualified Repo.Sql.LoginAttemptRepo as LoginAttemptRepo
import qualified Repo.Sql.RestrictedIpRepo as RestrictedIpRepo
import qualified Data.UUID as Uuid

import Config (redirectOnSuccess, connection, Config)
import Database.PostgreSQL.Simple (Connection)
import Data.UUID.V4 (nextRandom)
import Common.JWT (createJwt)
import Common.JWT (AuthToken(AuthToken), AuthToken(..), TokenUser(..))
import Jose.Jwt (unJwt, JwtError, Jwt)
import Web.Cookie (defaultSetCookie, setCookieValue, SetCookie(setCookieName))
import Web.Scotty.Cookie (setCookie)


defaultForm = LoginForm "" ""
defaultErrors = LoginErrors [] [] []


data AppState = AppState
  { users :: [User] }

routes :: (ScottyError e, MonadIO m) => Config -> IORef AppState -> ScottyT e m ()
routes config stateM = do
  let conn = connection config
  get "/signin" $ do
    html $ renderHtml $ layout "Sign In" [bootstrap3Link] [] (loginView defaultForm defaultErrors)
  post "/signin" $ do
    (view, result) <- runForm "signin-form" validateLogin
    case result of
      Just signInForm  -> do
        loginResult <- liftIO $ signIn (signInDepsFromConnection conn) (SignInDto (username signInForm) (password signInForm)) ""
        case loginResult of
          Left e -> do
            let loginErrors = defaultErrors { globalErrors = ["There was an error logging into your account"]}
            html $ renderHtml $ layout "Sign In" [bootstrap3Link] [] (loginView defaultForm loginErrors)
          Right loggedInUser -> do
            authTokenRes <- liftIO $ createLoginJwt config loggedInUser ()
            case authTokenRes of
              Left e -> text "there was a problem logging you in"
              Right jwt -> do
                setCookie $ defaultSetCookie { setCookieName = "sid", setCookieValue = unJwt jwt }
                redirect $ fromStrict $ redirectOnSuccess config
      Nothing -> 
        html $ renderHtml $ layout "Sign In" [bootstrap3Link] [] (loginView defaultForm (loginErrorsFromView view))

-- validateLogin :: Monad m => Form [Text] m LoginForm
-- validateLogin = LoginForm
--   <$> "username" .: validate (Form.text Nothing)
--   <*> "password" .: validate (Form.text Nothing)

loginErrorsFromView :: View [Text] -> LoginErrors
loginErrorsFromView view =
  LoginErrors [] (idxOr (errors "username" view) 0 []) (idxOr (errors "password" view ) 0 [])


signInDepsFromConnection :: Connection -> SignInDeps
signInDepsFromConnection conn
  = SignInDeps
  { signInDepsFindAllLoginAttemptsByIp = LoginAttemptRepo.findAllByIpAddress conn
  , signInDepsInsertLoginAttempt = LoginAttemptRepo.insert conn
  , signInDepsGenerateRestrictedIpUuid = \() -> nextRandom
  , signInDepsInsertRestrictedIp = RestrictedIpRepo.insert conn
  , signInDepsFindRestrictedIpByIp = RestrictedIpRepo.findByIpAddress conn
  , signInDepsFindUserByUsername = UserRepo.findByUsername conn
  , signInDepsCheckIfPasswordsMatch = \x y -> x == y  -- _todo: implement
  , signInDepsGenerateJwt = \() -> return "jwt token" -- _todo: implement
  }

createLoginJwt :: Config -> LoggedInUser -> () -> IO (Either JwtError Jwt)
createLoginJwt config user thunk = do
  authToken <- authTokenFromUser user
  createJwt config authToken

authTokenFromUser :: LoggedInUser -> IO AuthToken
authTokenFromUser user = do
  uuid <- nextRandom
  return $ AuthToken {
    authTokenId = Text.pack $ Uuid.toString uuid,
    authTokenUser = Just $ TokenUser {
      tokenUserId = Text.pack $ Uuid.toString $ _userId $ loggedInUserId user,
      roles = []
    } 
  }

