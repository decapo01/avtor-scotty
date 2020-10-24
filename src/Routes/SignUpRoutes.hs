{-# LANGUAGE OverloadedStrings #-}
module Routes.SignUpRoutes where

import Web.Scotty.Trans
import Control.Monad.IO.Class (liftIO, MonadIO)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Common.Views (bootstrap3Link, layout)
import Views.SignUpViews (SignUpFormErrors, SignUpFormErrors(..), SignUpForm, SignUpForm(..), signUpFormView, defaultSignUpFormErrors)
import Text.Digestive.Scotty (runForm)

import Data.Text (Text)
import Text.Digestive.Form (validate, (.:), Form)
import Validators (validatePassword, validateEmail)

import qualified Text.Digestive.Form as Form
import Text.Digestive.View (View, errors, fieldInputText)
import Common.Commons (idxOr)
import Text.Digestive (Result)
import Text.Digestive.Types (Result(Success))
import Text.Digestive (Result(Error))
import Avtor (AvtorError, signUpDtoPassword, signUpDtoEmail, SignUpDto(..))
import Data.UUID.V4 (nextRandom)

import qualified Repo.InMem.UserRepo as UserRepo

routes :: (ScottyError e, MonadIO m) => ScottyT e m ()
routes = do
  get "/blah" $ do
    html $ "blah"
  get "/signup" $ do
    html $ renderHtml $ layout "Sign Up" [bootstrap3Link] [] (signUpFormView (SignUpForm "" "" "") defaultSignUpFormErrors)
  post "/signup" $ do
    (view, result) <- runForm "signUpForm" validateSignUp
    case result of
      Just signUpForm -> do
        _ <- liftIO $ signUp createSignUpDto UserRepo.findByUsername hashPassword nextRandom nextRandom nextRandom
        redirect "/"
        where
          createSignUpDto = SignUpDto
            { signUpDtoEmail = username signUpForm
            , signUpDtoPassword = password signUpForm
            , signUpDtoConfirmPassword = confirmPassword signUpForm
            , signUpDtoAccountId = Nothing
            }
      Nothing -> html $ renderHtml $ layout "Sign Up" [bootstrap3Link] [] (signUpFormView (signUpFormFromView view) $ errorsFromView view)


hashPassword :: Text -> IO (Either AvtorError Text)
hashPassword pass = do
  return $ Right pass

validateSignUp :: Monad m => Form [Text] m SignUpForm
validateSignUp = SignUpForm
  <$> "username" .: validate validateEmail (Form.text Nothing)
  <*> "password" .: (Form.text Nothing)
  <*> "passwordGroup" .: vpass
  where
    vpass :: Monad m => Form [Text] m Text
    vpass = 
      validate fst' $ (,) <$> ("password" .: validate validatePassword (Form.text Nothing))
                          <*> ("confirmPassword" .: Form.text Nothing)
    fst' :: (Text, Text) -> Result [Text] Text
    fst' (p1, p2) =
      if p1 == p2
        then Success p1
        else Error ["passwords do not match"]

errorsFromView :: View [Text] -> SignUpFormErrors
errorsFromView view =
  SignUpFormErrors
    (idxOr (errors "username" view) 0 [])
    (idxOr (errors "passwordGroup.password" view) 0 [])
    (idxOr (errors "passwordGroup" view) 0 [])

signUpFormFromView :: View [Text] -> SignUpForm
signUpFormFromView view =
  SignUpForm
    (fieldInputText "username" view)
    (fieldInputText "passwordGroup.password" view)
    (fieldInputText "passwordGroup.confirmPassword" view)

validateConfirmPassword :: Text -> Text -> Result Text Text
validateConfirmPassword confirmPassword password =
  if password == confirmPassword
    then Success password
    else Error "passwords do not match"