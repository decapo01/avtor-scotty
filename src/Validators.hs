{-# LANGUAGE OverloadedStrings #-}
module Validators where

import Prelude hiding (length)
import Data.MonoTraversable
import Data.Maybe (isJust)
import Data.Text (Text, find, pack)
import qualified Data.Text as Txt

import Text.Digestive.Util
import Text.Digestive.Form
import Text.Digestive.Types
import Common.Views (LoginForm, LoginForm(..))

import Text.Regex


loginForm  = LoginForm "" ""

validateLogin :: Monad m => Form [Text] m LoginForm
validateLogin = LoginForm
  <$> "username" .: validate validateEmail (text Nothing)
  <*> "password" .: validate validatePassword (text Nothing)


checkEmail = isJust . find (== '@')

checkPassowrd password = Txt.length password >= 8 

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
  if Txt.length text /= 0
    then Success text
    else Error "cannot be empty"


matchesRegex :: a -> String -> Text -> Result a Text
matchesRegex errMsg regexStr txt =
  if isJust . matchRegex (mkRegexWithOpts regexStr True True) . Txt.unpack $ txt
    then Success txt
    else Error errMsg

validatePassword = conditions [notEmpty, minMaxLen 8 18, textContainsValidation ['!','@','#','$','^','&']]

validateEmail :: Text -> Result [Text] Text
validateEmail = conditions [matchesRegex "Not a valid email" "^[a-zA-Z0-9\\.\\+\\-]+@[a-zA-Z0-9]+\\.[a-zA-Z0-9]+$"]

textContains :: Text -> Char -> Bool
textContains text char =
  isJust $ find (== char) text

textContains_ :: [Char] -> Text  -> Bool
textContains_ chars text =
  foldr (\i a -> a || textContains text i) False chars

textContainsValidation :: [Char] -> Text -> Result Text Text
textContainsValidation chars text =
  if textContains_ chars text
    then Success text
    else Error $ Txt.pack $ "text must contain one of these symbols " ++ foldr (\x a -> [x] ++ ", " ++ a) "" chars