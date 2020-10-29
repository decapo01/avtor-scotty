module Config where

import Data.IORef
import Avtor (UnverifiedUser, User)
data Config
  = DbConfig {}
  | InMemConfig 
  { usersRef :: IORef [User]
  , unverifiedUsersRef :: IORef [UnverifiedUser]
  }