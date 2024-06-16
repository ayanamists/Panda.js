{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Panda.MetaData
  ( MetaData(..)
  , writeMeta
  ) where

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson
import Data.Aeson.Text (encodeToTextBuilder)

import Text.Pandoc.Writers.Shared (lookupMetaBool)
import Text.Pandoc.Definition
  ( lookupMeta
  , docAuthors
  , docDate
  , docTitle
  , Pandoc(..)
  , Meta(..)
  , MetaValue(..)
  )
import Text.Pandoc.Shared (stringify)

import GHC.Generics (Generic)

data MetaData = MetaData
  { authors :: [Text]
  , date :: Text
  , title :: Text
  , draft :: Bool
  , categories :: [Text]
  , tags :: [Text]
  } deriving (Generic, Show)

instance ToJSON MetaData where
  toEncoding = genericToEncoding defaultOptions

lookupMetaList :: Text -> Meta -> [MetaValue]
lookupMetaList key meta =
  case lookupMeta key meta of
    Just (MetaList l) -> l
    _ -> []

lookupMetaStringList :: Text -> Meta -> [Text]
lookupMetaStringList key = map stringify . lookupMetaList key

pandocToMeta :: Pandoc -> MetaData
pandocToMeta (Pandoc meta blocks) = MetaData
  { authors = authorsValue
  , date = stringify $ docDate meta
  , title = stringify $ docTitle meta
  , draft = draftValue
  , categories = categoriesValue
  , tags = tagsValue }
  where draftValue = lookupMetaBool "draft" meta
        authorsValue = stringify <$> docAuthors meta
        categoriesValue = lookupMetaStringList "categories" meta
        tagsValue = lookupMetaStringList "tags" meta

writeMeta :: Pandoc -> Text
writeMeta = write . pandocToMeta
  where write = toStrict . toLazyText . encodeToTextBuilder
