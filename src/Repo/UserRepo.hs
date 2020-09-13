{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Repo.UserRepo where

import Avtor (Account(..), User(..), UserId(..), AccountId(..))
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple.FromRow (field, fromRow)
import Database.PostgreSQL.Simple.ToField (Action(Escape), ToField, toField, Action)

import Data.UUID (toString, toASCIIBytes)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)

instance ToRow UserId
instance FromRow UserId

instance ToField UserId where
  toField uid = Escape $ toASCIIBytes $ _userId uid

instance FromField UserId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ UserId x

instance ToRow AccountId

instance ToField AccountId where
  toField aid = Escape $ toASCIIBytes $ _accountId aid

instance FromField AccountId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ AccountId x

instance ToRow User
instance FromRow User

instance ToRow Account where
  toRow a = [toField $ accountId a] <> [toField $ accountName a]

instance FromRow Account where
  fromRow = Account <$> field <*> field

-- idToRow :: User -> [Action]
-- idToRow u = [toField $ userId u]