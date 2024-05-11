{-# LANGUAGE OverloadedStrings #-}

module Panda.Pandoc2JSX
  ( writeJSX
  , defaultJSXWriterOptions
  ) where

import Text.Pandoc.Definition
import Text.Pandoc.Class
import Text.Pandoc.Walk (walkM)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Lazy (State, runState, get, put)
import Panda.JSX

newtype JSXWriterOptions = JSXWriterOptions {
    jsxWriterOptions :: String
}

defaultJSXWriterOptions :: JSXWriterOptions
defaultJSXWriterOptions = JSXWriterOptions {
    jsxWriterOptions = ""
}


getNoteIdName :: Int -> Text
getNoteIdName ctr = "note-" <> showT ctr

getNoteAnchorName :: Int -> Text
getNoteAnchorName ctr = "note-anchor-" <> showT ctr

noteDiv :: (Int, [Block]) -> Block
noteDiv (ctr, blocks) = Div (getNoteIdName ctr, ["note-item"], props) blocks
  where props = [("data-note-id", showT ctr)]

data NotePromptCtx = NotePromptCtx
  { counter :: Int
  , notes :: [(Int, [Block])]
  }

emptyNotePromptCtx :: NotePromptCtx
emptyNotePromptCtx = NotePromptCtx 1 []

nextNotePrompt :: [Block] -> State NotePromptCtx Int
nextNotePrompt blocks = do
  ctx <- get
  let ctr = counter ctx
  let nextCounter = ctr + 1
  put $ ctx { counter = nextCounter,
              notes = (ctr, blocks) : notes ctx }
  return ctr

notePrompt :: [Block] -> [Block]
notePrompt blocks = composeBlocks $ runState (notePromptBlock blocks) emptyNotePromptCtx
  where composeBlocks (_blocks, ctx) = _blocks ++
          [Div ("", ["notes"], []) $ map noteDiv . reverse $ notes ctx]
        notePromptBlock = walkM notePromptNote
        notePromptNote (Note _blocks) = do
          ctr <- nextNotePrompt _blocks
          return $ (Superscript [Link (getNoteAnchorName ctr, [], [])
                                 [Str $ T.pack $ show ctr] ("#" <> getNoteIdName ctr, "") ])
        notePromptNote inline = return inline



wrapDefs :: Text -> Text
wrapDefs body =
  T.intercalate "\n"
  [ "import {Fragment as _Fragment, jsx as _jsx, jsxs as _jsxs} from \"react/jsx-runtime\";"
  , "export default function MDXContent() { return " <> body <> "; }"
  ]

fullTag :: Text -> Text
fullTag tag = "_component." <> tag

wrapTag :: Text -> TagName
wrapTag tag = JSExpr $ fullTag tag <> ".tag"

withDefaultProps :: Text -> [JSXProp] -> [JSXProp]
withDefaultProps tag props = (RawProp $ "..." <> fullTag tag <> ".add_props"):props

emptyProps :: Text -> [JSXProp]
emptyProps tag = withDefaultProps tag []

writeAttr :: Attr -> [JSXProp]
writeAttr (idName, classes, kvs) = writeId idName ++ writeClasses classes ++ map writeKV kvs
  where
    writeKV (key, value) = MapProp (writeDoubleQuotesJSString key, writeDoubleQuotesJSString value)
    writeClasses []       = []
    writeClasses _classes = [MapProp ("className", writeJSString $ T.intercalate " " _classes)]
    writeId "" = []
    writeId _idName = [MapProp ("id", writeJSString _idName)]

writeAttrAndLink :: Attr -> Target -> [JSXProp]
writeAttrAndLink attr (url, _) = MapProp ("href", writeJSString url):writeAttr attr

buildJSXS :: JSX a => Text -> [JSXProp] -> [a] -> a
buildJSXS tag props = jsxs (wrapTag tag) (withDefaultProps tag props)

buildJSX :: JSX a => Text -> [JSXProp] -> a -> a
buildJSX tag props = jsx (wrapTag tag) (withDefaultProps tag props)

simpleJSX :: JSX a => Text -> a -> a
simpleJSX tag = buildJSX tag []

simpleJSXS :: JSX a => Text -> [a] -> a
simpleJSXS tag = buildJSXS tag []

simpleAttrJSX :: JSX a => Text -> Attr -> a -> a
simpleAttrJSX tag attr = buildJSX tag (writeAttr attr)

simpleAttrJSXS :: JSX a => Text -> Attr -> [a] -> a
simpleAttrJSXS tag attr = buildJSXS tag (writeAttr attr)

simpleEmtpyJSX :: JSX a => Text -> a
simpleEmtpyJSX tag = emptyJSX (wrapTag tag) (emptyProps tag)

_writeJSX :: JSX x => JSXWriterOptions -> Pandoc -> x
_writeJSX _ (Pandoc _ blocks) = jsxFragments [] $ map writeJSXBlocks $ notePrompt blocks

toJSXBlock :: JSX a => Text -> [Inline] -> a
toJSXBlock tag = simpleJSXS tag . map writeJSXInlines

toLiBlock :: JSX a => [Block] -> a
toLiBlock = simpleJSXS "li" . concatMap unwrapPlain
  -- a typical case is that
  -- - item1
  -- pandoc will parse it to BulletList [[Plain [Str "item1"]]]
  -- we need to unwrap the Plain block
  -- TODO: any other use cases for Plain block? Should we just convert it to a span?
  where unwrapPlain (Plain inlines) = map writeJSXInlines inlines
        unwrapPlain block = [writeJSXBlocks block]

isBlockInline :: Inline -> Bool
isBlockInline (Math DisplayMath _) = True
isBlockInline _ = False

writeJSXBlocks :: JSX x => Block -> x
writeJSXBlocks (Plain inlines) = toJSXBlock "div" inlines
writeJSXBlocks (Para inlines) = toJSXBlock tag inlines
  -- It's a hack, mainly for math blocks. E.g. $$ ... $$ in markdown
  -- (It should be a standalone block, but currently pandoc parse it to
  --  Para [Math DisplayMath ...])
  -- <p><div>...</div></p> should be avoided in JSX
  where tag = if any isBlockInline inlines then "div" else "p"
writeJSXBlocks (LineBlock _) = undefined
writeJSXBlocks (CodeBlock attr code) = simpleAttrJSX "pre" attr $ textJSX code
writeJSXBlocks (RawBlock _ _) = textJSX ""
writeJSXBlocks (BlockQuote blocks) = simpleJSXS "blockquote" $ map writeJSXBlocks blocks
writeJSXBlocks (OrderedList _ blocks) = simpleJSXS "ol" $ map toLiBlock blocks
writeJSXBlocks (BulletList blocks) = simpleJSXS "ul" $ map toLiBlock blocks
writeJSXBlocks (DefinitionList _) = undefined
writeJSXBlocks (Header level attr inlines) = simpleAttrJSXS ("h" <> T.pack (show level)) attr $ map writeJSXInlines inlines
writeJSXBlocks HorizontalRule = simpleEmtpyJSX "hr"
writeJSXBlocks (Table _ _ _ _ _ _) = undefined
writeJSXBlocks (Figure _ _ blocks) = simpleJSXS "figure" $ map writeJSXBlocks blocks
writeJSXBlocks (Div attr blocks) = simpleAttrJSXS "div" attr $ map writeJSXBlocks blocks

writeJSXInlines :: JSX x => Inline -> x
writeJSXInlines (Str text) = textJSX text
writeJSXInlines (Emph inlines) = simpleJSXS "em" $ map writeJSXInlines inlines
writeJSXInlines (Underline inlines) = simpleJSXS "u" $ map writeJSXInlines inlines
writeJSXInlines (Strong inlines) = simpleJSXS "strong" $ map writeJSXInlines inlines
writeJSXInlines (Strikeout inlines) = simpleJSXS "del" $ map writeJSXInlines inlines
writeJSXInlines (Superscript inlines) = simpleJSXS "sup" $ map writeJSXInlines inlines
writeJSXInlines (Subscript inlines) = simpleJSXS "sub" $ map writeJSXInlines inlines
writeJSXInlines (SmallCaps inlines) = simpleJSXS "small" $ map writeJSXInlines inlines
writeJSXInlines (Quoted _ inlines) = simpleJSXS "q" $ map writeJSXInlines inlines
writeJSXInlines (Cite _ inlines) = simpleJSXS "cite" $ map writeJSXInlines inlines
writeJSXInlines (Code attr text) = simpleAttrJSX "code" attr $ textJSX text
writeJSXInlines Space = textJSX " "
writeJSXInlines SoftBreak = textJSX " "
writeJSXInlines LineBreak = simpleEmtpyJSX "br"
writeJSXInlines (Math t text) = buildJSX "math" [MapProp ("display", disType t)] $ textJSX text
  where disType InlineMath = writeJSString "inline"
        disType DisplayMath = writeJSString "block"
writeJSXInlines (RawInline _ text) = textJSX text
writeJSXInlines (Link attr inlines target) = buildJSXS "a" (writeAttrAndLink attr target) $ map writeJSXInlines inlines
-- note: the image tag in JSX is self-closing
-- need some properly handling for the alt text
writeJSXInlines (Image attr _ target) = emptyJSX (wrapTag "img") props
  where props = withDefaultProps "img" $ [MapProp ("src", writeJSString $ fst target)] ++ writeAttr attr
writeJSXInlines (Note blocks) = simpleJSXS "note" $ map writeJSXBlocks blocks
writeJSXInlines (Span attr inlines) = simpleAttrJSXS "span" attr $ map writeJSXInlines inlines


writeJSX :: PandocMonad m => JSXWriterOptions -> Pandoc -> m Text
writeJSX opts doc = do
  let (r, _) = runState ((_writeJSX opts doc) :: JSXText) 0
  return r
