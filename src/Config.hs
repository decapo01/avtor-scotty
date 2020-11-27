module Config where

import Data.IORef
import Avtor (AccountId, UnverifiedUser, User)
import Database.PostgreSQL.Simple (Connection)

import Data.Text (Text)

data Config
  = DbConfig {}
  | InMemConfig 
  { usersRef :: IORef [User]
  , unverifiedUsersRef :: IORef [UnverifiedUser]
  , connection :: Connection
  , defaultAccountId :: AccountId
  , secretFile :: FilePath
  , redirectOnSuccess :: Text
  }