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
import Avtor (AccountId, userEmail, unverifiedUserToken, VerificationToken, verifyUser, unverifiedUserEmail, AvtorError(..), User, signUp, UserId, UnverifiedUser, AvtorError, signUpDtoPassword, signUpDtoEmail, SignUpDto(..), VerificationToken(..), _accountId)
import Data.UUID.V4 (nextRandom)

import qualified Repo.InMem.UserRepo as UserRepo
import qualified Repo.InMem.UnverifiedUserRepo as UnverifiedUserRepo
import Data.IORef (modifyIORef, newIORef, readIORef, IORef)

import qualified Data.Text.Lazy as LTxt
import qualified Data.Text as Txt
import qualified Data.UUID as Uuid
import qualified Data.List as List

import qualified Repo.Sql.UserRepo as UserSqlRepo
import qualified Repo.Sql.UnverifiedUserRepo as UnverifiedUserSqlRepo

import Data.UUID (UUID)
import Config (Config(..))
import Database.PostgreSQL.Simple (Connection)

data SignUpState = SignUpState
  { unverifiedUsers :: [UnverifiedUser]
  , users :: [User]
  }

-- data Config
--   = ProdConfig {}
--   | DevConfig
--   { getFineUserByEmail :: IORef [User] -> Text -> IO (Maybe User)
--   }

routes :: (ScottyError e, MonadIO m) => Config -> ScottyT e m ()
routes config = do
  get "/signup" $ do
    html $ renderHtml $ layout "Sign Up" [bootstrap3Link] [] (signUpFormView (SignUpForm "" "" "" (textFromAccountId $ defaultAccountId config)) defaultSignUpFormErrors)
  post "/signup" $ do
    (view, result) <- runForm "signUpForm" validateSignUp
    case result of
      Just signUpForm -> do
        liftIO $ print $ password signUpForm
        unverifiedUsersRef <- liftIO $ fetchUvUsers config
        -- todo: change this to take only one nextRandom
        signUpRes <- liftIO $ signUp createSignUpDto 
                                     (UserSqlRepo.findByUsername $ connection config) 
                                     hashPassword  
                                     genUuid  
                                     genUuid  
                                     genUuid  
                                     (UnverifiedUserSqlRepo.insert $ connection config) 
                                     sendEmail 
                                     (UnverifiedUserSqlRepo.remove $ connection config)
        case signUpRes of
          Right _ -> redirect "/"
          Left err ->
            case err of
              UserExists -> redirect "/"
              _ -> redirect "/"
        where
          -- todo: make mapper for this
          createSignUpDto = SignUpDto
            { signUpDtoEmail = username signUpForm
            , signUpDtoPassword = password signUpForm
            , signUpDtoConfirmPassword = confirmPassword signUpForm
            , signUpDtoAccountId = Uuid.fromText $ accountId signUpForm
            }
          userExistsErrors = defaultSignUpFormErrors { global = "User Exists" }
      Nothing -> html $ renderHtml $ layout "Sign Up" [bootstrap3Link] [] (signUpFormView (signUpFormFromView view) $ errorsFromView view)
  get "/verify/:token" $ do
    tokenText <- param "token"
    let tokenUuidMay = Uuid.fromText tokenText
    case tokenUuidMay of
      Nothing -> html "We had a problem processing your request"
      Just tokenUuid -> do
        verificationRes <- liftIO $ verifyUser (VerificationToken tokenUuid) (UnverifiedUserSqlRepo.findByToken (connection config)) (insertUserToDb (connection config))
        case verificationRes of
          Left e -> do
            liftIO $ putStrLn "user not found" 
            redirect "/"
          Right _ ->
            redirect "/"


insertUserToDb :: Connection -> User -> IO (Either AvtorError ())
insertUserToDb conn user = do
  _ <- UserSqlRepo.insert conn user
  return $ Right ()

fetchUvUsers :: Config -> IO [UnverifiedUser]
fetchUvUsers config =
  case config of
    c@InMemConfig{} -> readIORef $ (unverifiedUsersRef c)
    _ -> return $ []

fetchUsers :: Config -> IO [User]
fetchUsers c@InMemConfig{} = readIORef $ usersRef $ c
fetchUsers _ = return []


findByUsername :: Config -> Text -> IO (Maybe User)

findByUsername c@InMemConfig{} uname = do
  us <- readIORef $ usersRef $ c
  return $ List.find (\u -> (userEmail u) == uname) us

findByUsername _ uname = return $ Nothing

findByToken :: Config -> VerificationToken -> IO (Maybe UnverifiedUser)
findByToken c@InMemConfig{} token = do
  UnverifiedUserRepo.findByToken (unverifiedUsersRef c) token

findByToken _ token = return Nothing


insertUvUser :: Config -> UnverifiedUser -> IO (Either AvtorError ())
insertUvUser c@InMemConfig{} uvUser = do
  UnverifiedUserRepo.insert (unverifiedUsersRef c) uvUser
  return $ Right ()

insertUvUser _ uvUser = return  $ Right ()

-- todo: implement this
hashPassword :: Text -> IO (Either AvtorError Text)
hashPassword pass = do
  return $ Right pass

validateSignUp :: Monad m => Form [Text] m SignUpForm
validateSignUp = SignUpForm
  <$> "username" .: validate validateEmail (Form.text Nothing)
  <*> "passwordGroup.password" .: (Form.text Nothing)
  <*> "passwordGroup" .: vpass
  <*> "account_id" .: (Form.text Nothing)
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
    ""

signUpFormFromView :: View [Text] -> SignUpForm
signUpFormFromView view =
  SignUpForm
    (fieldInputText "username" view)
    (fieldInputText "passwordGroup.password" view)
    (fieldInputText "passwordGroup.confirmPassword" view)
    (fieldInputText "account_id" view)

validateConfirmPassword :: Text -> Text -> Result Text Text
validateConfirmPassword confirmPassword password =
  if password == confirmPassword
    then Success password
    else Error "passwords do not match"

-- todo: make maybe error in avtor core
insertUnverifiedUser :: IORef [UnverifiedUser] -> UnverifiedUser -> IO (Either AvtorError ())
insertUnverifiedUser ref user = do
  UnverifiedUserRepo.insert ref user
  return $ Right ()

-- todo: really implement these two functions
sendEmail :: Text -> IO (Either AvtorError ())
sendEmail emailBody = do
  return $ Right  ()

removeEmailIfSendFails :: UserId -> IO (Either AvtorError ())
removeEmailIfSendFails userId =
  return $ Right ()

genUuid :: () -> IO UUID
genUuid _ = nextRandom

insertUser :: Config -> User -> IO (Either AvtorError ())
insertUser c@InMemConfig{} user = do
  UserRepo.insert (usersRef c) user
  return $ Right ()

insertUser _ user = return $ Right ()

textFromAccountId :: AccountId -> Text
textFromAccountId aid = Uuid.toText $ _accountId aid