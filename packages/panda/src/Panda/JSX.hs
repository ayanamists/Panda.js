{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable  #-}

module Panda.JSX
  ( TagName(..)
  , JSX(..)
  , JSXProp(..)
  , JSXText
  , jsxFragments
  , showT
  , wrapText
  , writeJSArray
  , writeJSString
  , writeDoubleQuotesJSString
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Lazy (State, get, put, runState)

data JSXProp = RawProp Text | MapProp (Text, Text)

data TagName = RawString Text | JSExpr Text

class JSX a where
  textJSX :: Text -> a
  emptyJSX :: TagName -> [JSXProp] -> a
  jsx :: TagName -> [JSXProp] -> a -> a
  jsxs :: TagName -> [JSXProp] -> [a] -> a

wrapText :: Text -> Text
wrapText text = "\"" <> text <> "\""

showT:: Show a => a -> Text
showT = T.pack . show

writeJSArray :: [Text] -> Text
writeJSArray arr = "[" <> (T.intercalate ", " $ map wrapText arr) <> "]"

writeJSString :: Text -> Text
writeJSString str = "`" <> rep str <> "`"
  where rep = T.replace "`" "\\`" . T.replace "${" "\\${" . T.replace "\\" "\\\\"

-- TODO: it's not safe, figure out how to correctly escape this
--       not just replace double quotes with single quotes, also \n, \t, etc.
writeDoubleQuotesJSString :: Text -> Text
writeDoubleQuotesJSString str = "\"" <> str <> "\""

tagNameToText :: TagName -> Text
tagNameToText (RawString text) = wrapText text
tagNameToText (JSExpr text) = text

data Children a = Empty | Child a | Children [a]
  deriving (Functor, Foldable, Traversable)

childrenToText :: Children Text -> Text
childrenToText (Child child) = "children:" <> child
childrenToText Empty = ""
childrenToText (Children children) = "children:[" <> T.intercalate ", " children <> "]"

jsxToText :: Text -> Int -> TagName -> [JSXProp] -> Children Text -> Text
jsxToText func key tagName props children =
  func <> "("
  <> tagNameToText tagName <> ", "
    <> "{"
    <> propsToText props
    <> childrenToText children
    <> "}, "
    <> showT key
  <> ")"

propsToText :: [JSXProp] -> Text
propsToText [] = ""
propsToText props = T.intercalate ", " (map propToText props) <> ","
  where
    propToText (RawProp text) = text
    propToText (MapProp (key, value)) = key <> ": " <> value

jsxFragments :: JSX a => [JSXProp] -> [a] -> a
jsxFragments = jsxs (JSExpr "_Fragment")

type JSXText = State Int Text

getKey :: State Int Int
getKey = do
  key <- get
  put $ key + 1
  return key

buildJSXText :: Text -> TagName -> [JSXProp] -> Children JSXText -> JSXText
buildJSXText t tagName props children = do
  key <- getKey
  let (children', _) = runState (sequence children) 0
  return $ jsxToText t key tagName props children'

instance JSX JSXText where
  textJSX = return . writeJSString
  emptyJSX tagName props = buildJSXText "_jsx" tagName props Empty
  jsx tagName props child = buildJSXText "_jsx" tagName props (Child child)
  jsxs tagName props children = buildJSXText "_jsxs" tagName props (Children children)
