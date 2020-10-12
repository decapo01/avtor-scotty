{-# LANGUAGE OverloadedStrings #-}
module Validators where

import Prelude hiding (length)
import Data.MonoTraversable
import Data.Maybe (isJust)
import Data.Text (Text, find, pack)
import qualified Data.Text as Text

import Text.Digestive.Util
import Text.Digestive.Form
import Text.Digestive.Types
import Common.Views (LoginForm, LoginForm(..))


loginForm  = LoginForm "" ""

validateLogin :: Monad m => Form [Text] m LoginForm
validateLogin = LoginForm
  <$> "username" .: check ["Invalid Username"] checkEmail (text Nothing)
  <*> "password" .: validate validatePassword (text Nothing)


checkEmail = isJust . find (== '@')

checkPassowrd password = Text.length password >= 8 

-- todo: move to common area
tshow :: Show a => a -> Text
tshow item = pack $ show item

minMaxLen :: MonoFoldable a => Int -> Int -> a -> Result Text a
minMaxLen min max item =
  if olength item >= min && olength item <= max
    then Success item
    else Error $ "Item should be between " <> tshow min <> " and " <> tshow max <> " length"


notEmpty :: Text -> Result Text Text
notEmpty text =
  if Text.length text /= 0
    then Success text
    else Error "cannot be empty"


validatePassword = conditions [notEmpty, minMaxLen 8 18]