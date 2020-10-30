{-# LANGUAGE OverloadedStrings #-}
module EndToEnd.Registration where

import Test.WebDriver

ffConfig :: WDConfig
ffConfig = useBrowser chrome defaultConfig { wdHTTPRetryCount = 50 }


registrationTest = runSession ffConfig $ do

  openPage "http://localhost:3000/signup"
  
  signUpFormElem <- findElem $ ById "signUpForm"
  usernameElem <- findElem $ ByName "signUpForm.username"
  passwordElem <- findElem $ ByName "signUpForm.passwordGroup.password"
  confirmElem <- findElem $ ByName "signUpForm.passwordGroup.confirmPassword"

  sendKeys "blah@blah.com" usernameElem
  sendKeys "!Q2w3e4r5t" passwordElem
  sendKeys "!Q2w3e4r5t" confirmElem

  submit signUpFormElem
  
  closeSession
  
