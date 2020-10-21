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

import qualified Data.List as List
import Avtor (User(User))
import Web.Scotty.Trans
import Web.Scotty.Internal.Types (ScottyError)
import Control.Monad.IO.Class (MonadIO)

import qualified Web.Scotty as Scotty
import Data.IORef (IORef)
import Common.Commons (idxOr)


defaultForm = LoginForm "" ""
defaultErrors = LoginErrors [] []


data AppState = AppState
  { users :: [User] }

routes :: (ScottyError e, MonadIO m) => IORef AppState -> ScottyT e m ()
routes stateM = do
  get "/signin" $ do
    html $ renderHtml $ layout "Sign In" [bootstrap3Link] [] (loginView defaultForm defaultErrors)
  post "/signin" $ do
    (view, result) <- runForm "signin-form" validateLogin
    case result of
      Just _  -> redirect "/"
      Nothing -> html $ renderHtml $ layout "Sign In" [bootstrap3Link] [] (loginView defaultForm (loginErrorsFromView view))

loginErrorsFromView :: View [Text] -> LoginErrors
loginErrorsFromView view =
  LoginErrors (idxOr (errors "username" view) 0 []) (idxOr (errors "password" view ) 0 [])