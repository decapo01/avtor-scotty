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
  , global :: Text
  }

defaultSignUpFormErrors = SignUpFormErrors [] [] [] ""

signUpFormView :: SignUpForm  -> SignUpFormErrors -> Markup
signUpFormView signUpForm errors =
  div ! class_ "container" $ do
    form ! method "POST" $ do
      textInput "Username" "signUpForm.username" (attrFromText (username signUpForm)) (usernameErrors errors)
      passwordInput "Password" "signUpForm.passwordGroup.password" (attrFromText (password signUpForm)) (passwordErrors errors)
      passwordInput "Confirm Password" "signUpForm.passwordGroup.confirmPassword" (attrFromText (confirmPassword signUpForm)) (confirmPasswordErrors errors)
      div ! class_ "form-group" $ do
        input ! type_ "submit" ! class_ "btn btn-primary" ! value "Sign Up"
    
type Label = Text
type Name = AttributeValue
type Value = AttributeValue
type ErrMsgs = [Text]

textInput :: Label -> Name -> Value -> ErrMsgs -> Markup
textInput label_ name_ value_ errMsgs = do
  div ! class_ "form-group" $ do
    label ! class_ (mkLabel errMsgs) $ text label_
    input ! class_ (mkFormControl errMsgs) ! name name_  ! type_ "text" ! value value_
    div ! class_ "text-danger" $ do
      ul $ do
        forM_ errMsgs $ \errMsg ->
          li $ text errMsg

passwordInput :: Label -> Name -> Value -> ErrMsgs -> Markup
passwordInput label_ name_ value_ errMsgs = do
  div ! class_ "form-group" $ do
    label ! class_ (mkLabel errMsgs) $ text label_
    input ! class_ (mkFormControl errMsgs) ! name name_  ! type_ "password" ! value value_
    div ! class_ "text-danger" $ do
      ul $ do
        forM_ errMsgs $ \errMsg ->
          li $ text errMsg

mkLabel :: [Text] -> AttributeValue
mkLabel msgs =
  Str.fromString $ Txt.unpack "control-label " <>
    if (length msgs) == 0
      then ""
      else "text-danger"

mkFormControl :: [Text] -> AttributeValue
mkFormControl msgs =
  Str.fromString $ Txt.unpack "form-control " <>
    if (length msgs) == 0
      then ""
      else "is-invalid"

isInvalid :: [Text] -> AttributeValue
isInvalid errorMsgs =
  Str.fromString $ Txt.unpack $ if (length errorMsgs) == 0
    then ""
    else "is-invalid"

attrFromText :: Text -> AttributeValue
attrFromText text_ = Str.fromString $ Txt.unpack text_