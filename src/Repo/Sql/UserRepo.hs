{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
module Repo.Sql.UserRepo where

import Repo.Sql.AccountRepo

import DbCommon as GenericRepo
import DbCommon (Updateable, toUpdateRow_)
import Avtor (Account(..), User(..), UserId(..), AccountId(..))
import Database.PostgreSQL.Simple (query, FromRow, ToRow)
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.FromRow (field, fromRow)
import Database.PostgreSQL.Simple.ToField (Action(Escape), ToField, toField, Action)

import Data.UUID (toString, toASCIIBytes)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.Internal (Connection)

import qualified Data.Text as Txt
import Database.PostgreSQL.Simple.Types

instance ToRow UserId
instance FromRow UserId

instance ToField UserId where
  toField uid = Escape $ toASCIIBytes $ _userId uid

instance FromField UserId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ UserId x

instance ToRow User
instance FromRow User

instance Insertable User where
  toInsertRow_ u = toRow u

instance Updateable User where
  toUpdateRow_ u = 
    [ toField $ userEmail $ u
    , toField $ userPass  $ u
    , toField $ userAccountId $ u
    ]

userTable = Table "users"

findById :: Connection -> UserId -> IO (Maybe User)
findById conn userId = 
  GenericRepo.findById conn userTable userId


findByUsername :: Connection -> Text -> IO (Maybe User)
findByUsername conn username = do
  users <- query conn "select * from users where username = ? limit 1" (Only username)
  return $ if length users == 0
    then Nothing
    else Just $ users !! 0

findAll :: Connection -> IO [User]
findAll conn =
  GenericRepo.findAll conn userTable

insert :: Connection -> User -> IO ()
insert conn user =
  GenericRepo.insert conn "insert into users values (?, ?, ?, ?)" user

update :: Connection -> User -> IO ()
update conn user =
  GenericRepo.update conn "update user set name = ? password = ? account_id = ? where id = ?" user

delete :: Connection -> UserId -> IO ()
delete conn userId =
  GenericRepo.delete conn "delete from users where id = ?" userId