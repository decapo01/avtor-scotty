module Repo.Sql.UnverifiedUserRepo where

import Avtor (AvtorError, VerificationToken, verificationToken, VerificationToken(..), UnverifiedUser, UserId(..), UnverifiedUser(..), VerToken(..), AccountId(..))

import Database.PostgreSQL.Simple.ToField (Action(Escape), toField, ToField)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToRow (toRow)
import Database.PostgreSQL.Simple (query, ToRow)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)

import Data.UUID (toASCIIBytes)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.Internal (Connection)

import qualified DbCommon as GenericRepo
import qualified Data.String as String
import DbCommon (Insertable)
import Database.PostgreSQL.Simple.Types (Only(Only))

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

instance Insertable UnverifiedUser where
  toInsertRow_ a = toRow a

table = "unverified_users"

insert :: Connection -> UnverifiedUser -> IO (Either AvtorError ())
insert conn uvUser = do
  _ <- GenericRepo.insert conn (String.fromString insertQuery) uvUser
  return $ Right ()
  where
    insertQuery = "insert into " ++ table ++ " (id, username, password, token, account_id) values (?, ?, ?, ?, ?)"


remove :: Connection -> UserId -> IO (Either AvtorError ())
remove conn userId = do
  _ <- GenericRepo.delete conn (String.fromString "") userId
  return $ Right ()
  where
    deleteQuery = "delete from " ++ table ++ " where id = ?"


findByToken :: Connection -> VerificationToken -> IO (Maybe UnverifiedUser)
findByToken conn vToken = do
  uvUsers <- query conn queryString (Only $ verificationToken vToken)
  return $ if (length uvUsers) == 0
    then Nothing
    else Just $ uvUsers !! 0
  where
    queryString = String.fromString ("select * from " ++ table ++ " where token = ? ") 