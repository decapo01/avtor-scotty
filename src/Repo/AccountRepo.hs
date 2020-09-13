{-# LANGUAGE OverloadedStrings #-}
module Repo.AccountRepo where

import DbCommon (Updateable, toUpdateRow_)

import DbCommon as GenericRepo

import Avtor (Account(..), AccountId(..))
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Database.PostgreSQL.Simple.ToField (Action(Escape), ToField, toField)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Data.UUID (toASCIIBytes)
import Database.PostgreSQL.Simple.Internal (Connection)


instance ToRow Account where
  toRow a = [toField $ accountId a] <> [toField $ accountName a]

instance Updateable Account where
  toUpdateRow_ a = [toField $ accountName a] <> [toField $ accountName a]

instance FromRow Account where
  fromRow = Account <$> field <*> field

instance ToRow AccountId
instance FromRow AccountId

instance ToField AccountId where
  toField aid = Escape $ toASCIIBytes $ _accountId aid

instance FromField AccountId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ AccountId x

instance Insertable Account where
  toInsertRow_ a = toRow a

findById :: Connection -> AccountId -> IO (Maybe Account)
findById conn accountId =
  GenericRepo.findById conn "select * from accounts where id = ?" accountId

findAll :: Connection -> IO [Account]
findAll conn =
  GenericRepo.findAll conn "select * from accounts"

insert :: Connection -> Account -> IO ()
insert conn account =
  GenericRepo.insert conn "insert into accounts values (? , ?)" account

update :: Connection -> Account -> IO ()
update conn account =
  GenericRepo.update conn "update accounts set name = ? where id = ?" account

delete :: Connection -> AccountId -> IO ()
delete conn accountId =
  GenericRepo.delete conn "delete from accounts where id = ?" accountId