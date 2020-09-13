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

class ToRow a => Id a b | a -> b where
  id_ :: a -> b

class (ToRow a, FromRow a, Insertable a, Updateable a, ToField b, ToRow b, Id b c) => Entity a b c | a -> b where
  eid :: a -> b

  toIdRow :: a -> [Action]
  toBaseRow :: a -> [Action]

  toInsertRow :: a -> [Action]
  toInsertRow e = toIdRow e <> toBaseRow e

findAll :: (FromRow entity) => Connection -> Query -> IO [entity]
findAll conn queryStr =
  query_ conn queryStr

findById :: (ToRow id, FromRow entity) => Connection -> Query -> id -> IO (Maybe entity)
findById conn queryStr id = do
  xs <- query conn queryStr id
  return $ if List.length xs == 0
    then Nothing
    else Just $ xs !! 0

insert :: (Insertable item) => Connection -> Query -> item -> IO ()
insert conn queryStr item = do
  _ <- execute conn queryStr $ toInsertRow_ item
  return ()

update :: (Updateable item) => Connection -> Query -> item -> IO ()
update conn queryStr item = do
  _ <- execute conn queryStr $ toUpdateRow_ item
  return ()

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

instance Insertable Item where
  toInsertRow_ i = [toField $ itemId i] <> [toField $ itemName i]

instance Updateable Item where
  toUpdateRow_ i = [toField $ itemName i] <> [toField $ itemId i]

itemFindById conn id = findById conn "select * from items where id = ?" id
itemFindAll conn = findAll conn "select * from items"
itemInsert conn item = insert conn "insert into items (?, ?)" item
itemUpdate conn item = update conn "update items set name = ? where id = ?" item

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