{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Repo.Sql.Migrations.Migration01 where

import Text.RawString.QQ

migration01 = [r|

insert into migrations (id, description) values (
  1,
  'creating tables accounts, users, unverified_users & login_attemts'
);

create table if not exists accounts (
  id uuid unique primary key,
  name varchar(255) not null  
);

create table if not exists users (
  id uuid unique primary key,
  username varchar(255) not null,
  password varchar(255) not null,
  account_id uuid references accounts(id)
);

create table if not exists unverified_users (
  id uuid unique primary key,
  username varchar(255) not null,
  password varchar(255) not null,
  account_id uuid references accounts(id)
);

create table if not exists login_attempts (
  id uuid unique primary key,
  ip varchar(255) not null,
  username varchar(255) not null,
  success bool not null  
);
|]


downSql = [r|

drop table accounts;

drop table users;

drop table unverified_users;

drop table login_attempts;

|]