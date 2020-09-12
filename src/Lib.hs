{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Data.Text
import Avtor (SignInDto(..))

s = SignInDto { signInDtoEmail = "", signInDtoPassword = ""}

someFunc :: IO ()
someFunc = putStrLn "someFunc"
