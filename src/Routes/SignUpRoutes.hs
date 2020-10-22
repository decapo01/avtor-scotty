{-# LANGUAGE OverloadedStrings #-}
module Routes.SignUpRoutes where

import Web.Scotty.Trans
import Control.Monad.IO.Class (MonadIO)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Common.Views (bootstrap3Link, layout)
import Views.SignUpViews (SignUpFormErrors, SignUpFormErrors(..), SignUpForm, SignUpForm(..), signUpFormView, defaultSignUpFormErrors)
import Text.Digestive.Scotty (runForm)
-- import Text.Digestive.Blaze.Html5 (inputText)

import Data.Text (Text)
import Text.Digestive.Form (validate, (.:), Form)
import Validators (validatePassword, validateEmail)

import qualified Text.Digestive.Form as Form
import Text.Digestive.View (View, errors, fieldInputText)
import Common.Commons (idxOr)
import Text.Digestive (Result)
import Text.Digestive.Types (Result(Success))
import Text.Digestive (Result(Error))

routes :: (ScottyError e, MonadIO m) => ScottyT e m ()
routes = do
  get "/blah" $ do
    html $ "blah"
  get "/signup" $ do
    html $ renderHtml $ layout "Sign Up" [bootstrap3Link] [] (signUpFormView (SignUpForm "" "" "") defaultSignUpFormErrors)
  post "/signup" $ do
    (view, result) <- runForm "signUpForm" validateSignUp
    case result of
      Just _ -> redirect "/"
      Nothing -> html $ renderHtml $ layout "Sign Up" [bootstrap3Link] [] (signUpFormView (signUpFormFromView view) $ errorsFromView view)


validateSignUp :: Monad m => Form [Text] m SignUpForm
validateSignUp = SignUpForm
  <$> "username" .: validate validateEmail (Form.text Nothing)
  <*> "confirmPassword" .: validate validateConfirmPassword $ (,)
    <$> "password" .: (Form.text Nothing)
    <*> "confirmPassword" .: (Form.text Nothing)

errorsFromView :: View [Text] -> SignUpFormErrors
errorsFromView view =
  SignUpFormErrors
    (idxOr (errors "username" view) 0 [])
    (idxOr (errors "password" view) 0 [])
    (idxOr (errors "confirmPassword" view) 0 [])

signUpFormFromView :: View [Text] -> SignUpForm
signUpFormFromView view =
  SignUpForm
    (fieldInputText "username" view)
    (fieldInputText "password" view)
    (fieldInputText "confirmPassword" view)

validateConfirmPassword :: Text -> Text -> Result Text Text
validateConfirmPassword confirmPassword password =
  if password == confirmPassword
    then Success password
    else Error "passwords do not match"