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

import qualified Data.List as List


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

idxOr :: [a] -> Int -> a -> a
idxOr items idx alt =
    if List.length items  >= (idx + 1)
      then items !! idx
      else alt

loginErrorsFromView :: View [Text] -> LoginErrors
loginErrorsFromView view =
  LoginErrors (idxOr (errors "username" view) 0 []) (idxOr (errors "password" view ) 0 [])