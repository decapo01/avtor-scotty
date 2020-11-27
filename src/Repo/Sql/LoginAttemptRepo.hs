{-# LANGUAGE OverloadedStrings #-}
module Repo.Sql.LoginAttemptRepo where

import Avtor (LoginAttempt, LoginAttemptId, _loginAttemptId, loginAttemptId, address, createdOn, LoginAttempt(..), LoginAttemptId(..))

import Data.UUID (toASCIIBytes)

import GHC.Generics (Generic)

import Database.PostgreSQL.Simple.ToField (Action(Escape), toField, ToField)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple (Only(..), query, Connection, ToRow)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)

import qualified DbCommon as GenRepo
import Data.Text (Text)

instance ToField LoginAttemptId where
  toField lid = Escape $ toASCIIBytes $ _loginAttemptId lid

instance FromField LoginAttemptId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ LoginAttemptId x

instance ToRow LoginAttemptId

instance ToRow LoginAttempt where
  toRow la =
    [toField $ loginAttemptId la] <>
    [toField $ address la] <>
    [toField $ createdOn la]

instance FromRow LoginAttemptId

instance FromRow LoginAttempt where
  fromRow = LoginAttempt <$> field <*> field <*> field

instance GenRepo.Insertable LoginAttempt where
  toInsertRow_ = toRow

loginAttemptsTable = GenRepo.Table "login_attempts"

insert :: Connection -> LoginAttempt -> IO ()
insert conn loginAttempt =
  GenRepo.insert conn "insert into login_attempts (id, address, created_on) values (?, ?, ?)" loginAttempt

findAllByIpAddress :: Connection -> Text -> IO ([LoginAttempt])
findAllByIpAddress conn ip = do
  query conn "select * from login_attempts where ip = ?" (Only ip)