{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Repo.InMem.UnverifiedUserRepo where

import Data.Text (Text)
import Avtor (VerificationToken, verToken, VerToken, unverifiedUserToken, UnverifiedUser, _userId, unverifiedUserId)
import Repo.InMem.GenericRepo (RefIdAble, refId)

import qualified Data.UUID as Uuid
import qualified Repo.InMem.GenericRepo as GenRepo
import qualified Data.List as List

import Data.IORef (IORef, readIORef)

instance RefIdAble UnverifiedUser Text where
  refId user = Uuid.toText $ _userId $ unverifiedUserId user

insert :: IORef [UnverifiedUser] -> UnverifiedUser -> IO ()
insert usersRef user =
  GenRepo.insert usersRef user

findByToken :: IORef [UnverifiedUser] -> VerificationToken -> IO (Maybe UnverifiedUser)
findByToken uvUsersRef token = do
  uvUsers <- readIORef uvUsersRef
  return $ List.find (\u -> (unverifiedUserToken u) == token) uvUsers