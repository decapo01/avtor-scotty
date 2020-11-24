{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Repo.Sql.Migrations.Migrations where

import Text.RawString.QQ

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, execute)

import qualified Data.String as String
import Control.Monad (forM_)


import qualified Data.Text as Txt

data Migration
  = Migration
  { id :: Integer
  , description :: Text
  }

migrationsCreationSql = [r|

create table if not exists migrations (
  id integer,
  description varchar(255)  
);

|]

startMigration :: Connection -> IO ()
startMigration conn = do
  execute conn (String.fromString migrationsCreationSql) ()
  return ()

applyMigrations :: Connection -> [String] -> IO ()
applyMigrations conn migrationSqls = do
  forM_ migrationSqls $ \m -> do
    execute conn (String.fromString m) ()