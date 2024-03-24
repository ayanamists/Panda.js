{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( writeJSX
    , defaultJSXWriterOptions
    ) where

import Text.Pandoc.Definition
import Text.Pandoc.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Panda.JSX

data JSXWriterOptions = JSXWriterOptions {
    jsxWriterOptions :: String
}

defaultJSXWriterOptions :: JSXWriterOptions
defaultJSXWriterOptions = JSXWriterOptions {
    jsxWriterOptions = ""
}


wrapDefs :: Text -> Text
wrapDefs body =
  T.intercalate "\n"
  [ "import {Fragment as _Fragment, jsx as _jsx, jsxs as _jsxs} from \"react/jsx-runtime\";"
  , "export default function MDXContent() { return " <> body <> "; }"
  ]

writeJSX :: (PandocMonad m, JSX x) => JSXWriterOptions -> Pandoc -> m x
writeJSX _ (Pandoc _ blocks) = return $ jsxFragments $ map writeJSXBlocks blocks

writeJSXBlocks :: JSX x => Block -> x
writeJSXBlocks (Para inlines) = jsxs "p" [] $ writeJSXInlines <$> inlines

writeJSXInlines :: JSX x => Inline -> x
writeJSXInlines (Str text) = textJSX text
writeJSXInlines Space = textJSX " "


