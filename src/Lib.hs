{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Web.Scotty (scotty)
import Routes.Routes (routes)
import Data.Text
import Avtor (SignInDto(..))
import DbCommon (findAll, update, findById, createDbQuery, createQuery, insert, Item(..), ItemId(..), Entity, itemTable)
import Database.PostgreSQL.Simple (execute_, connect, defaultConnectInfo, connectHost, connectDatabase, connectUser, connectPassword)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Blah.Blah (foo, blah)
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


someFunc :: IO ()
someFunc = do
  scotty 3000 $ do
    routes
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
