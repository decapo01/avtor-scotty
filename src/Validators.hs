{-# LANGUAGE OverloadedStrings #-}
module Validators where

import Prelude hiding (length)

import Data.Maybe (isJust)
import Data.Text (Text, find, length)
import Text.Digestive.Form
import Common.Views (LoginForm, LoginForm(..))


loginForm  = LoginForm "" ""

validateLogin :: Monad m => Form Text m LoginForm
validateLogin = LoginForm
  <$> "username" .: check "Invalid Username" checkEmail (text Nothing)
  <*> "password" .: check "Invalid Password" checkPassowrd (text Nothing)


checkEmail = isJust . find (== '@')

checkPassowrd password = length password >= 8 