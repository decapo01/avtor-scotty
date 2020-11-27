{-# LANGUAGE OverloadedStrings #-}
module Repo.Sql.RestrictedIpRepo where

import Data.UUID (toASCIIBytes)
import Database.PostgreSQL.Simple.ToField (Action(Escape), ToField, toField)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.FromRow (field, FromRow, fromRow)
import Avtor (RestrictedIp, RestrictedIp(..), RestrictedIpId, RestrictedIpId(..))

import qualified DbCommon as GenRepo
import Database.PostgreSQL.Simple (query, Connection)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Types (Only(Only))

instance ToField RestrictedIpId where
  toField lid = Escape $ toASCIIBytes $ _restrictedIpId lid

instance FromField RestrictedIpId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ RestrictedIpId x

instance ToRow RestrictedIpId
instance FromRow RestrictedIpId

instance ToRow RestrictedIp where
  toRow la =
    [toField $ restrictedIpId la] <>
    [toField $ restrictedIp la]

instance FromRow RestrictedIp where
  fromRow = RestrictedIp <$> field <*> field

instance GenRepo.Insertable RestrictedIp where
  toInsertRow_ = toRow

restrictedIpsTable = GenRepo.Table "restricted_ips"

insert :: Connection -> RestrictedIp -> IO ()
insert conn loginAttempt =
  GenRepo.insert conn "insert into restricted_ips (id, ip) values (?, ?)" loginAttempt

findByIpAddress :: Connection -> Text -> IO (Maybe RestrictedIp)
findByIpAddress conn ip = do
  ips <- query conn "select * from login_attempts where ip = ? limit 1" (Only ip)
  return $ if (length ips) == 0
    then Nothing
    else Just $ ips !! 0