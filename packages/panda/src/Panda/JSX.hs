{-# LANGUAGE OverloadedStrings #-}

module Panda.JSX (JSX(..), jsxFragments) where

import Data.Text (Text)
import qualified Data.Text as T

type JSXProps = [(Text, Text)]

type TagName = Text

class JSX a where
  textJSX :: Text -> a
  emptyJSX :: TagName -> JSXProps -> a
  jsx :: TagName -> JSXProps -> a -> a
  jsxs :: TagName -> JSXProps -> [a] -> a
  jsxFragments :: [a] -> a

jsxToText :: Text -> TagName -> JSXProps -> [Text] -> Text
jsxToText func tagName props children =
  func <> "("
  <> tagName <> ", " <> "{"
    <> propsToText props
    <> childrenToText children <> "}"
  <> ")"

propsToText :: JSXProps -> Text
propsToText props = T.intercalate ", " [key <> ": " <> value | (key, value) <- props]

wrapText :: Text -> Text
wrapText text = "\"" <> text <> "\""

childrenToText :: [Text] -> Text
childrenToText [] = ""
childrenToText children = "children: [" <> T.intercalate ", " children <> "]"

instance JSX Text where
  textJSX = wrapText
  emptyJSX tagName props = jsxToText "_jsx" (wrapText tagName) props []
  jsx tagName props child = jsxToText "_jsx" (wrapText tagName) props [child]
  jsxs tagName props children = jsxToText "_jsxs" (wrapText tagName) props children
  jsxFragments = jsxToText "_jsxs" "_Fragment" []
