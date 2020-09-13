{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module DbCommon where

import Data.Text
import Database.PostgreSQL.Simple (execute, query_, Query, query, Connection, FromRow, ToRow)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.ToField (Action(Escape), ToField, toField)
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)

import qualified Data.List as List

class Insertable a where
  toInsertRow_ :: a -> [Action]

class Updateable a where
  toUpdateRow_ :: a -> [Action]

class Id a b | a -> b where
  id_ :: a -> b

class (ToRow a, FromRow a, ToField b, ToRow b, Id b c) => Entity a b c | a -> b where
  eid :: a -> b

  toIdRow :: a -> [Action]
  toBaseRow :: a -> [Action]

  selectAllQuery :: a -> Query

  findAllSql :: a -> Connection -> IO [a]
  findAllSql e conn =
    query_ conn $ selectAllQuery e

  selectByIdQuery :: a -> Query

  findById :: a -> Connection -> IO (Maybe a)
  findById e conn = do 
    xs <- query conn (selectByIdQuery e) (eid e)
    if List.length xs == 0
      then return Nothing
      else return $ Just $ xs !! 0

  toInsertRow :: a -> [Action]
  toInsertRow e = toIdRow e <> toBaseRow e

  insertQuery :: a -> Query

  insert :: a -> Connection -> IO ()
  insert e conn = do
    _ <- execute conn (insertQuery e) e
    return $ ()

  toUpdateRow :: a -> [Action]
  toUpdateRow e = toBaseRow e <> toIdRow e

  updateQuery :: a -> Query

  update :: a -> Connection -> IO ()
  update e conn = do
    _ <- execute conn (updateQuery e) e
    return $ ()

findById_ :: (ToRow id, FromRow entity) => Connection -> Query -> id -> IO (Maybe entity)
findById_ conn queryStr id = do
  xs <- query conn queryStr id
  return $ if List.length xs == 0
    then Nothing
    else Just $ xs !! 0

update_ :: (Updateable item) => Connection -> Query -> item -> IO ()
update_ conn queryStr item = do
  _ <- execute conn queryStr $ toUpdateRow_ item
  return $ ()

  -- findByIdSql :: a -> Text
  -- findByIdSql e = findAllSql e <> " where id = ?"


newtype ItemId = ItemId 
  { _itemId :: Text
  }
  deriving (Generic, ToRow)

instance ToField ItemId where
  toField i = Escape $ encodeUtf8 $ _itemId i

instance FromField ItemId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ ItemId x


instance Id ItemId Text where
  id_ = _itemId

data Item = Item
  { itemId :: ItemId
  , itemName :: Text
  }
  deriving (Generic, ToRow, FromRow)

instance Entity Item ItemId Text where
  eid = itemId

  toIdRow i = [toField $ itemId i]
  toBaseRow i = [toField $ itemName i]

  selectAllQuery i = "select * from items"
  selectByIdQuery i = "select * from items where id = ?"
  insertQuery _ = "insert into items values (?, ?)"
  updateQuery _ = "update items set name = ? where id = ?"

instance Insertable Item where
  toInsertRow_ i = [toField $ itemId i] <> [toField $ itemName i]

instance Updateable Item where
  toUpdateRow_ i = [toField $ itemName i] <> [toField $ itemId i]

itemId_ :: Item -> ItemId
itemId_ i = itemId i


item = Item { itemId = ItemId "", itemName = "" }

createDbQuery :: Query
createDbQuery = "\
\create database itemsdb \
\"

createQuery :: Query
createQuery = "\
\create table if not exists Items ( \
\  id varchar(255) not null primary key, \
\  name varchar(255) not null \
\)\ 
\"