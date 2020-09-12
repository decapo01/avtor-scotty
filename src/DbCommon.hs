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

class Id a b | a -> b where
  id_ :: a -> b

class (ToRow a, FromRow a, ToField b, ToRow b, Id b c) => Entity a b c | a -> b where
  eid :: a -> b
  table :: a -> Text

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

  insertQuery :: a -> Query

  insert :: a -> Connection -> IO ()
  insert e conn = do
    _ <- execute conn (insertQuery e) e
    return $ ()

  updateQuery :: a -> Query

  update :: a -> Connection -> IO ()
  update e conn = do
    _ <- execute conn (updateQuery e) e
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
  table _ = "items"

itemId_ :: Item -> ItemId
itemId_ i = itemId i


item = Item { itemId = ItemId "", itemName = "" }


-- data ItemEntity = EData Item ItemId {

-- }

-- itemEntity :: Item -> EData Item ItemId
-- itemEntity item = EData {
--   Eeid = itemId item,
--   table_ = "items",
--   insertFields_  = "",
--   updateFields_ = ""
-- }

-- findAllItems = findAllSql itemEntity

-- instance Entity a where
--   eid = itemId
--   table _ = "items"
--   insertFieldQs _ = "(?, ?)"
--   updateFieldQs _ = "id = ? , name = ?"FromField ItemId => 