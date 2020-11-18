{-# LANGUAGE OverloadedStrings #-}
module EndToEnd.Registration where

import Test.WebDriver

import Network.HTTP.Req
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson
import Test.Hspec
import Control.Monad (forM_, liftM)

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig { wdHTTPRetryCount = 50 }

instance MonadHttp WD

baseUrl = "http://localhost:3000"

registrationTest = runSession chromeConfig $ do

  openPage $ baseUrl ++ "/signup"
  
  signUpFormElem <- findElem $ ById "signUpForm"
  usernameElem <- findElem $ ByName "signUpForm.username"
  passwordElem <- findElem $ ByName "signUpForm.passwordGroup.password"
  confirmElem <- findElem $ ByName "signUpForm.passwordGroup.confirmPassword"

  sendKeys "blah@blah.com" usernameElem
  sendKeys "!Q2w3e4r5t" passwordElem
  sendKeys "!Q2w3e4r5t" confirmElem

  submit signUpFormElem

  openPage $ baseUrl ++ "/unverified-users"

  closeSession

registrationFailTest = hspec $ do
  describe "Registration Fails" $ do
    it "should succeed" $ do
      runSession chromeConfig $ do
        openPage $ baseUrl ++ "/signup"

        signUpFormElem <- findElem $ ById "signUpForm"
        usernameElem <- findElem $ ByName "signUpForm.username"
        passwordElem <- findElem $ ByName "signUpForm.passwordGroup.password"
        confirmElem <- findElem $ ByName "signUpForm.passwordGroup.confirmPassword"

        sendKeys "blah@blah.com" usernameElem
        sendKeys "!Q2w3e4r5t" passwordElem
        sendKeys "!Q2w3e4r5t" confirmElem

        submit signUpFormElem

        currentUrl <- getCurrentURL

        liftIO $ currentUrl `shouldBe` (baseUrl ++ "/")

        closeSession
    it "should fail" $ do
      runSession chromeConfig $ do

        openPage $ baseUrl ++ "/signup"

        signUpFormElem <- findElem $ ById "signUpForm"
        usernameElem <- findElem $ ByName "signUpForm.username"
        passwordElem <- findElem $ ByName "signUpForm.passwordGroup.password"
        confirmElem <- findElem $ ByName "signUpForm.passwordGroup.confirmPassword"

        sendKeys "blah@blah.com" usernameElem
        sendKeys "" passwordElem
        sendKeys "" confirmElem

        submit signUpFormElem

        currentUrl <- getCurrentURL

        liftIO $ currentUrl `shouldBe` (baseUrl ++ "/signup")

        passwordErrorElems <- findElems $ ByClass "password-error"

        liftIO $ (length passwordErrorElems) `shouldBe` 3

        firstPwErrMsg <- getText $ passwordErrorElems !! 0

        liftIO $ firstPwErrMsg `shouldBe` ("cannot be empty")

        closeSession

  

-- blah :: IO Int
-- blah = runReq defaultHttpConfig $ do
--   res <- req GET (http "localhost:3000/unverified-users") NoReqBody jsonResponse mempty
--   return $ (responseBody res :: Int)
--   where
--     payload = object
--       [ "foo" .= (20 :: Int)
--       , "bar" .= (10 :: Int)
--       ]