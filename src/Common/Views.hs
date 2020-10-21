{-# LANGUAGE OverloadedStrings #-}
module Common.Views where

import Prelude hiding (head, div)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)

import Data.Text (Text)
import Control.Monad (forM_)

import qualified Data.Text as Txt
import qualified Data.String as Str


layout :: Text -> [Markup] -> [Markup] -> Markup -> Markup
layout title_ stylesheets scripts content = do
  html $ do
    head $ do
      title $ text title_
      forM_ stylesheets $ \s -> s
    body $ do
      content
      forM_ scripts $ \s -> s


bootstrap3Link =
  link ! href "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css" ! rel "stylesheet"

data LoginForm
  = LoginForm
  { username :: Text
  , password :: Text
  }

data LoginErrors
  = LoginErrors
  { usernameErrors :: [Text]
  , passwordErrors :: [Text]
  }


loginView :: LoginForm -> LoginErrors -> Markup
loginView loginForm loginErrors =
  div ! class_ "container" $ do
    div ! class_ "row" $ do
      div ! class_ "col-lg-12" $ do
        loginComponent loginForm loginErrors


loginComponent :: LoginForm -> LoginErrors -> Markup
loginComponent loginForm loginErrors =
  form ! method "POST" $ do
    div ! class_ "form-group" $ do
      label ! class_ "control-label" $ text "Username"
      input ! name "signin-form.username" ! class_ "form-control" ! type_ "text" ! value uname
      div $ do
        ul $ do
          forM_ (usernameErrors loginErrors) $ \e ->
            li $ text e
    div ! class_ "form-group" $ do
      label ! class_ "control-label" $ text "Password"
      input ! name "signin-form.password" ! class_ "form-control" ! type_ "password" ! value pword
      div $ do
        ul $ do
          forM_ (passwordErrors loginErrors) $ \e ->
            li $ text e
    div ! class_ "form-group" $ do
      input ! class_ "btn btn-primary" ! type_ "submit" ! value "Sign In"
  where
    uname = Str.fromString $ Txt.unpack $ username loginForm
    pword = Str.fromString $ Txt.unpack $ password loginForm