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
import qualified Data.String as String

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

newtype Table 
  = Table
  { tableValue :: String
  }

findAllQuery :: Table -> String
findAllQuery table = String.fromString $ "select * from " ++ tableValue table

findByIdQuery :: Table -> String
findByIdQuery table = String.fromString $ findAllQuery table ++ " where id = ?"

findAll :: (FromRow entity) => Connection -> Table -> IO [entity]
findAll conn table =
  query_ conn $ String.fromString $ findAllQuery table

findById :: (ToRow id, FromRow entity) => Connection -> Table -> id -> IO (Maybe entity)
findById conn table id = do
  xs <- query conn (String.fromString $ findByIdQuery table) id
  return $ if List.length xs == 0
    then Nothing
    else Just $ xs !! 0

insert_ :: (Insertable item) => Connection -> Table -> String -> String -> item -> IO ()
insert_ conn table columns paramString item = do
  _ <- execute conn qString $ toInsertRow_ item
  return ()
  where
    qString = String.fromString $ "insert into " ++ tableValue table ++ columns ++ " values " ++ paramString

insert :: (Insertable item) => Connection -> Query -> item -> IO ()
insert conn queryStr item = do
  _ <- execute conn queryStr $ toInsertRow_ item
  return ()

update :: (Updateable item) => Connection -> Query -> item -> IO ()
update conn queryStr item = do
  _ <- execute conn queryStr $ toUpdateRow_ item
  return ()

update_ :: (Updateable item) => Connection -> Table -> String -> String -> item -> IO ()
update_ conn table columns params item = do
  _ <- execute conn qStr $ toUpdateRow_ item
  return ()
  where
    qStr = String.fromString $ "update " ++ tableValue table ++ " set " ++ columns ++ " where id = ?"

delete :: (ToRow id) => Connection -> Query -> id -> IO ()
delete conn queryStr id = do
  _ <- execute conn queryStr id
  return ()

delete_ :: (ToRow id) => Connection -> Table -> id -> IO ()
delete_ conn table id = do
  _ <- execute conn qStr id
  return ()
  where
    qStr = String.fromString $ "delete from " ++ tableValue table ++ " where id = ?"

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

itemTable = Table "items"

itemFindById conn id = findById conn itemTable id
itemFindAll conn = findAll conn itemTable
itemInsert conn item = insert conn "insert into items (?, ?)" item
itemUpdate :: Updateable item => Connection -> item -> IO ()
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