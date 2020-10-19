{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Repo.InMem.UserRepo where

import Data.Text (Text)
import Avtor (User, userEmail, UserId, userId, _userId)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.List (filter, find)
import Repo.InMem.GenericRepo (RefIdAble)

import qualified Repo.InMem.GenericRepo as GenRepo

import qualified Data.UUID as Uuid

-- todo: create eq for UserId
instance RefIdAble User Text where
  refId user = Uuid.toText $ _userId $ userId user


findByUsername :: IORef [User] -> Text -> IO (Maybe User)
findByUsername usersRef username = do
  users <- readIORef usersRef
  return $ find (\u -> (userEmail u) == username) users

insert :: IORef [User] -> User -> IO ()
insert usersRef user =
  GenRepo.insert usersRef user 
  -- modifyIORef usersRef (\us -> user : us)

update :: IORef [User] -> User -> IO ()
update usersRef user = do
  GenRepo.update usersRef user

remove :: IORef [User] -> Text -> IO ()
remove usersRef uId =
  GenRepo.delete usersRef uId