{-# LANGUAGE OverloadedStrings #-}
module Routes.Routes where

import Data.Text (Text, unpack)
import Data.Text.Lazy (toStrict, fromStrict)
import Web.Scotty
import Common.Views (layout, loginView, bootstrap3Link, LoginForm(..), LoginErrors(..))

import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Blaze.ByteString.Builder (Builder)

import Text.Digestive.Scotty
import Validators (validateLogin)
import Text.Digestive.View (absoluteRef, View)
import Text.Digestive (errors)


defaultForm = LoginForm "" ""
defaultErrors = LoginErrors [] []

routes = do
  get "/signin" $ do
    html $ renderHtml $ layout "Sign In" [bootstrap3Link] [] (loginView defaultForm defaultErrors)
  post "/signin" $ do
    (view, result) <- runForm "signin-form" validateLogin
    case result of
      Just _  -> redirect "/"
      Nothing -> html $ renderHtml $ layout "Sign In" [bootstrap3Link] [] (loginView defaultForm (loginErrorsFromView view))


loginErrorsFromView :: View Text -> LoginErrors
loginErrorsFromView view =
  LoginErrors (errors "username" view) (errors "password" view)
