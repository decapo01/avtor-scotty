{-# LANGUAGE OverloadedStrings #-}
module InMem.UserRepo where

import Data.Text (Text)
import Avtor (User, userEmail)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.List (filter, find)


findByUsername :: IORef [User] -> Text -> IO (Maybe User)
findByUsername usersRef username = do
  users <- readIORef usersRef
  return $ find (\u -> (userEmail u) == username) users

insert :: IORef [User] -> User -> IO ()
insert usersRef user = do
  modifyIORef usersRef (\us -> us ++ [user])