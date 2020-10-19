{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleContexts #-}
module Repo.InMem.GenericRepo where

import Data.List (find)
import Data.IORef (readIORef, IORef, modifyIORef)
import Data.Text (Text)

class (Eq b) => RefIdAble a b | a -> b where
  refId :: a -> b

findById :: RefIdAble a b => IORef [a] -> b -> IO (Maybe a)
findById itemRef itemId = do
  items <- readIORef itemRef
  return $ find (\i -> (refId i) == itemId) items

insert :: IORef [a] -> a -> IO ()
insert itemRef item = do
  modifyIORef itemRef (\items -> item : items)

update :: RefIdAble a b => IORef [a] -> a -> IO ()
update itemRef item =
  modifyIORef itemRef (\is -> item : (filter (\i -> (refId i) == (refId item)) is))

delete :: RefIdAble a b => IORef [a] -> b -> IO ()
delete itemRef itemId = do
  modifyIORef itemRef (\is -> filter (\i -> (refId i) == itemId) is)