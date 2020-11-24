module Repo.Sql.UnverifiedUserRepo where

import Avtor (VerificationToken, verificationToken, VerificationToken(..), UnverifiedUser, UserId(..), UnverifiedUser(..), VerToken(..), AccountId(..))

import Database.PostgreSQL.Simple.ToField (Action(Escape), toField, ToField)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)

import Data.UUID (toASCIIBytes)
import GHC.Generics (Generic)

instance ToField UserId where
  toField uid = Escape $ toASCIIBytes $ _userId uid

instance ToField VerToken where
  toField t = Escape $ toASCIIBytes $ verToken t

instance ToField VerificationToken where
  toField t = Escape $ toASCIIBytes $ verificationToken t


instance ToField AccountId where
  toField aId = Escape $ toASCIIBytes $ _accountId aId

instance ToRow UserId

instance ToRow UnverifiedUser where
  toRow u = 
    [toField $ unverifiedUserId u] <> 
    [toField $ unverifiedUserEmail u] <>
    [toField $ unverifiedUserPassword u] <>
    [toField $ unverifiedUserToken u] <>
    [toField $ unverifiedUserAccountId u]


instance FromRow UserId
instance FromRow VerToken
instance FromRow AccountId

instance FromField UserId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ UserId x

instance FromField VerToken where
  fromField field mdata = do
    x <- fromField field mdata
    return $ VerToken x

instance FromField VerificationToken where
  fromField field mdata = do
    x <- fromField field mdata
    return $ VerificationToken x

instance FromField AccountId where
  fromField field mdata = do
    x <- fromField field mdata
    return $ AccountId x

instance FromRow UnverifiedUser where
  fromRow = UnverifiedUser <$> field <*> field <*> field <*> field <*> field