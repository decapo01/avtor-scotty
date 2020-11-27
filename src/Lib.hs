{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Web.Scotty (scotty)
import Routes.Routes (routes)
import Data.Text
import Avtor (Account, Account(..), AccountId(..), User, SignInDto(..))
import DbCommon (findAll, update, findById, createDbQuery, createQuery, insert, Item(..), ItemId(..), Entity, itemTable)
import Database.PostgreSQL.Simple (execute_, connect, defaultConnectInfo, connectHost, connectDatabase, connectUser, connectPassword)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Blah.Blah (foo, blah)
import GHC.IORef (IORef(IORef))

import qualified Routes.Routes as MyRoutes
import qualified Data.UUID as Uuid
import qualified Repo.Sql.AccountRepo as AccountRepo

import qualified Routes.SignUpRoutes as SignUpRoutes
import Data.IORef (newIORef)
import Config
import Repo.Sql.Migrations.Migrations (startMigration, applyMigrations)
import Repo.Sql.Migrations.Migration01 (migration01)
import Database.PostgreSQL.Simple.Internal (Connection)
import Data.UUID (UUID)

-- import Blah.Blah (foo, blah)

s = SignInDto { signInDtoEmail = "", signInDtoPassword = ""}

item1 = Item { itemId = ItemId "1", itemName = "item1" }
item2 = Item { itemId = ItemId "2", itemName = "item2" }

tihif :: String -> String
tihif i = blah i ++ blah "slkdfj"


bar = foo ""

-- this_is_how_i_feel :: String -> String
-- this_is_how_i_feel b = blah b

items =
  [ item1
  , item2
  ]

pgConnection =
  connect $ defaultConnectInfo
  { connectHost = "localhost"
  , connectUser = "postgres"
  , connectPassword = "postgres"
  }

itemsConnection =
  connect $ defaultConnectInfo 
  { connectHost = "localhost"
  , connectDatabase = "itemsdb"
  , connectUser = "postgres"
  , connectPassword = "postgres"
  }


users :: [User]
users = []

appState = MyRoutes.AppState []

signUpState = SignUpRoutes.SignUpState [] []

defAccountId = "cec8c966-a6bb-4c38-b0d7-bcb430fae6ae"

defaultAccount uuid = Account {
  accountId = AccountId $ uuid,
  accountName = "default"
}

insertDefaultAccount :: Connection -> IO (Maybe UUID)
insertDefaultAccount conn =
  case Uuid.fromString defAccountId of
    Nothing -> return $ Nothing
    Just uuid -> do
      existingAccount <- AccountRepo.findById conn (AccountId uuid)
      case existingAccount of
        Just e -> return $ Just uuid
        Nothing -> do
          _ <- AccountRepo.insert conn (defaultAccount uuid)
          return $ Just uuid

someFunc :: IO ()
someFunc = do
  pgConn <- pgConnection
  _ <- startMigration pgConn
  _ <- applyMigrations pgConn [migration01]
  successMaybe <- insertDefaultAccount pgConn
  case successMaybe of
    Nothing -> putStrLn "default account could not be created"
    Just uuid -> do
      initialState <- newIORef appState
      signUpRef    <- newIORef signUpState
      usersRef <- newIORef []
      uvUsersRef <- newIORef []
      let config = InMemConfig usersRef uvUsersRef pgConn (AccountId uuid) "secret" "/"
      scotty 3000 $ do
        routes config initialState
        SignUpRoutes.routes config
  -- pgConn <- pgConnection
  -- execute_ pgConn createDbQuery
  -- itemsConn <- itemsConnection
  -- execute_ itemsConn createQuery
  -- forM_ items $ \i -> insert i itemsConn
  -- update_ itemsConn "update items set name = ? where id = ?" $ Item (ItemId "1") "Updated Item"
  -- maybeItem <- findById_ itemsConn "select * from items where id = ?" $ ItemId "1"
  -- case maybeItem of
  --   Just i -> putStrLn $ unpack $ itemName i
  --   Nothing -> putStrLn "no item foumnd"
  -- items_ <- findAll itemsConn itemTable
  -- forM_ items_ $ \i -> putStrLn $ unpack $ itemName i
