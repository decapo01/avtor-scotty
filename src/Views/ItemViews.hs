{-# LANGUAGE OverloadedStrings #-}
module Views.ItemViews where

import Prelude hiding (div, head)

import Data.Text hiding (head)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title)
import DbCommon (Item, Item(..), ItemId(..))
import Control.Monad (forM_)

layout :: Text -> Markup -> Markup
layout title_ content = html $ do
  head $ do
    title $ text title_
  body $ do
    content

rootView :: [Item] -> Markup
rootView items = do
  layout "Items" $
    div ! class_ "container" $ do
      div ! class_ "row" $ do
        div ! class_ "col-lg-12" $ do 
          h1 $ text "Items"
      div ! class_ "row" $ do
        div ! class_ "col-lg-12" $ do
          a ! class_ "btn btn-default" ! href "#" $ text "Create"
      div ! class_ "row" $ do
        div ! class_ "col-lg-12" $ do
          table $ do
            thead $ do
              tr $ do
                th $ text "Id"
                th $ text "Name"
                th $ text ""
                th $ text ""
            tbody $ do
              forM_ items $ \i -> do
                tr $ do
                  td $ text $ _itemId $ itemId i
                tr $ do
                  td $ text $ itemName i
                tr $ do
                  td $ a ! class_ "btn btn-primary" ! href "#" $ text "Edit"
                tr $ do
                  td $ a ! class_ "btn btn-primary" ! href "#" $ text "Delete"
              