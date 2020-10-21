{-# LANGUAGE OverloadedStrings #-}
module Views.SignUpViews where

import Prelude hiding (head, div)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)

import Data.Text (Text)
import Control.Monad (forM_)

import qualified Data.String as Str
import qualified Data.Text as Txt

data SignUpForm = SignUpForm
  { username :: Text
  , password :: Text
  , confirmPassword :: Text
  }

data SignUpFormErrors = SignUpFormErrors
  { usernameErrors :: [Text]
  , passwordErrors :: [Text]
  , confirmPasswordErrors :: [Text]
  }

defaultSignUpFormErrors = SignUpFormErrors [] [] []

signUpFormView :: SignUpForm  -> SignUpFormErrors -> Markup
signUpFormView signUpForm errors =
  div ! class_ "container" $ do
    form ! method "POST" $ do
      div ! class_ "form-group" $ do
        label ! class_ "control-label" $ text "Username"
        input ! class_ "form-control" ! type_ "text" ! name "signUpForm.username" ! value uname
        ul $ do
          forM_ (usernameErrors errors) $ \e ->
            li $ text e
      div ! class_ "form-group" $ do
        label ! class_ "control-label" $ text "Password"
        input ! class_ "form-control" ! type_ "text" ! name "signUpForm.password" ! value pword
        ul $ do
          forM_ (passwordErrors errors) $ \e ->
            li $ text e
      div ! class_ "form-group" $ do
        label ! class_ "control-label" $ text "Confirm Password"
        input ! class_ "form-control" ! type_ "text" ! name "signUpForm.confirmPassword" ! value cword
        ul $ do
          forM_ (confirmPasswordErrors errors) $ \e ->
            li $ text e
      div ! class_ "form-group" $ do
        input ! type_ "submit" ! class_ "btn btn-primary" ! value "Sign Up"
  where
    uname = Str.fromString $ Txt.unpack $ username signUpForm
    pword = Str.fromString $ Txt.unpack $ password signUpForm
    cword = Str.fromString $ Txt.unpack $ confirmPassword signUpForm
    