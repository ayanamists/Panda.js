{-# LANGUAGE OverloadedStrings #-}

module Panda.Pandoc2JSX
  ( writeJSX
  , defaultJSXWriterOptions
  ) where

import qualified Text.Pandoc.Writers.AnnotatedTable as Ann
import Data.Default (def)
import Text.Pandoc.Definition
import Text.Pandoc.Class
import Text.Pandoc.Writers.Shared (toTableOfContents)
import Text.Pandoc.Walk (walkM, walk)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Lazy (State, runState, get, put)
import Data.Maybe (catMaybes)
import Data.Char (chr)
import Panda.JSX
import Panda.CJK

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
          return $ Superscript [Link (getNoteAnchorName ctr, [], [])
                                 [Str $ T.pack $ show ctr] ("#" <> getNoteIdName ctr, "") ]
        notePromptNote inline = return inline

rawBlockMerge :: [Block] -> [Block]
rawBlockMerge [] = []
rawBlockMerge (RawBlock (Format "html") t1 : RawBlock (Format "html") t2 : xs) =
  rawBlockMerge $ RawBlock (Format "html") (t1 <> t2) : xs
rawBlockMerge (x:xs) = x:rawBlockMerge xs

wrapOrgResult :: [Block] -> [Block]
wrapOrgResult (c@(CodeBlock (_, _, kvs) _):t:bs)
  | ("exports", "both") `elem` kvs =
    Div ("", ["code-with-result"], [])
      [c, Div ("", ["org-result"], []) [t]]
    :wrapOrgResult bs
wrapOrgResult (b:bs) = b:wrapOrgResult bs
wrapOrgResult []     = []

getTOC :: [Block] -> Block
getTOC blocks = Div ("toc", ["toc"], []) [toTableOfContents def blocks]

fullTag :: Text -> Text
fullTag tag = "_component." <> tag

wrapTag :: Text -> TagName
wrapTag tag = JSExpr $ fullTag tag <> ".tag"

withDefaultProps :: Text -> [JSXProp] -> [JSXProp]
withDefaultProps tag props = RawProp ("..." <> fullTag tag <> ".add_props"):props

emptyProps :: Text -> [JSXProp]
emptyProps tag = withDefaultProps tag []

writeAttr :: Attr -> [JSXProp]
writeAttr (idName, classes, kvs) = writeId idName ++ writeClasses classes ++ map writeKV kvs
  where
    writeKV (key, value) = MapProp (writeDoubleQuotesJSString key, RawString value)
    writeClasses []       = []
    writeClasses _classes = [
      MapProp ("className", JSExpr $ writeJSString $ T.intercalate " " _classes)]
    writeId "" = []
    writeId _idName = [MapProp ("id", JSExpr $ writeJSString _idName)]

writeAttrAndLink :: Attr -> Target -> [JSXProp]
writeAttrAndLink attr (url, _) = MapProp
  ("href", JSExpr $ writeJSString url):writeAttr attr

buildJSXS :: JSX a => Text -> [JSXProp] -> [a] -> a
buildJSXS tag props = jsxs (wrapTag tag) (withDefaultProps tag props)

buildJSX :: JSX a => Text -> [JSXProp] -> a -> a
buildJSX tag props = jsx (wrapTag tag) (withDefaultProps tag props)

simpleJSX :: JSX a => Text -> a -> a
simpleJSX tag = buildJSX tag []

simpleAttrJSX :: JSX a => Text -> Attr -> a -> a
simpleAttrJSX tag attr = buildJSX tag (writeAttr attr)

simpleEmtpyJSX :: JSX a => Text -> a
simpleEmtpyJSX tag = emptyJSX (wrapTag tag) (emptyProps tag)

simpleJSXS :: JSX a => Text -> [a] -> a
simpleJSXS tag = buildJSXS tag []

simpleAttrJSXS :: JSX a => Text -> Attr -> [a] -> a
simpleAttrJSXS tag attr = buildJSXS tag (writeAttr attr)

_writeJSX :: JSX x => JSXWriterOptions -> Pandoc -> x
_writeJSX _ (Pandoc _ blocks) = jsxFragments [] [toc, article]
  where article = simpleJSXS "article" .
          map writeJSXBlocks
          . processCJK
          . walk wrapOrgResult
          . notePrompt
          . rawBlockMerge
          $ blocks
        toc = writeJSXBlocks . getTOC $ blocks

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
writeJSXBlocks (RawBlock (Format "html") str) = simpleJSX "rawhtml" $ textJSX str
writeJSXBlocks (RawBlock _ _) = textJSX ""
writeJSXBlocks (BlockQuote blocks) = simpleJSXS "blockquote" $ map writeJSXBlocks blocks
writeJSXBlocks (OrderedList _ blocks) = simpleJSXS "ol" $ map toLiBlock blocks
writeJSXBlocks (BulletList blocks) = simpleJSXS "ul" $ map toLiBlock blocks
writeJSXBlocks (DefinitionList _) = undefined
writeJSXBlocks (Header level attr inlines) = simpleAttrJSXS ("h" <> T.pack (show level)) attr $ map writeJSXInlines inlines
writeJSXBlocks HorizontalRule = simpleEmtpyJSX "hr"
writeJSXBlocks (Table attr caption colspecs thead tbody tfoot) =
  tableToJSX (Ann.toTable attr caption colspecs thead tbody tfoot)
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
writeJSXInlines (Math t text) = buildJSX "math"
  [MapProp ("display", JSExpr $ disType t)] $ textJSX text
  where disType InlineMath = writeJSString "inline"
        disType DisplayMath = writeJSString "block"
writeJSXInlines (RawInline _ text) = textJSX text
writeJSXInlines (Link attr inlines target) = buildJSXS "a" (writeAttrAndLink attr target) $ map writeJSXInlines inlines
-- note: the image tag in JSX is self-closing
-- need some properly handling for the alt text
writeJSXInlines (Image attr _ target) = emptyJSX (wrapTag "img") props
  where props = withDefaultProps "img" $
          MapProp ("src", JSExpr $ writeJSString $ fst target):writeAttr attr
writeJSXInlines (Note blocks) = simpleJSXS "note" $ map writeJSXBlocks blocks
writeJSXInlines (Span attr inlines) = simpleAttrJSXS "span" attr $ map writeJSXInlines inlines

writeJSX :: PandocMonad m => JSXWriterOptions -> Pandoc -> m Text
writeJSX opts doc = do
  let (r, _) = runState (_writeJSX opts doc :: JSXText) 0
  return r

-- | The part of a table; header, footer, or body.
data TablePart = Thead | Tfoot | Tbody
  deriving (Eq)

data CellType = HeaderCell | BodyCell

data TableRow = TableRow TablePart Attr Ann.RowNumber Ann.RowHead Ann.RowBody

tableToJSX :: JSX a => Ann.Table -> a
tableToJSX (Ann.Table attr caption _ thead tbodies tfoot) =
  simpleAttrJSXS "table" attr
    (catMaybes [ tableCaption caption
    , tableHead thead
    ] ++ tableBodies tbodies ++
    catMaybes [tableFoot tfoot])

tableCaption :: JSX a => Caption -> Maybe a
tableCaption (Caption _ []) = Nothing
tableCaption (Caption _ longCapt) = Just $
  simpleJSXS "caption" . map writeJSXBlocks $ longCapt

tableHead :: JSX a => Ann.TableHead -> Maybe a
tableHead (Ann.TableHead attr rows) =
  tablePartToJSX Thead attr rows

tableFoot :: JSX a => Ann.TableFoot -> Maybe a
tableFoot (Ann.TableFoot attr rows) =
  tablePartToJSX Tfoot attr rows

tablePartToJSX :: JSX a => TablePart -> Attr -> [Ann.HeaderRow] -> Maybe a
tablePartToJSX tblpart attr rows
  | null rows || all isEmptyRow rows = Nothing
  | otherwise = Just $ simpleAttrJSXS tag attr $ headerRowsToJSX tblpart rows
    where
      isEmptyRow (Ann.HeaderRow _attr _rownum cells) = all isEmptyCell cells
      isEmptyCell (Ann.Cell _colspecs _colnum cell) =
        cell == Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) []
      tag = case tblpart of
                 Thead -> "thead"
                 Tfoot -> "tfoot"
                 Tbody -> "tbody"

tableBodies :: JSX a => [Ann.TableBody] -> [a]
tableBodies = map tableBodyToJSX

tableBodyToJSX :: JSX a => Ann.TableBody -> a
tableBodyToJSX (Ann.TableBody attr _rowHeadCols inthead rows) =
  simpleAttrJSXS "tbody" attr
    (intermediateHead [inthead] ++ bodyRowsToJSX rows)
  where
    intermediateHead _inthead
      | null _inthead = []
      | otherwise = concatMap (headerRowsToJSX Thead) _inthead


headerRowsToJSX :: JSX a => TablePart -> [Ann.HeaderRow] -> [a]
headerRowsToJSX tablepart =
  map (tableRowToJSX . toTableRow)
  where
    toTableRow (Ann.HeaderRow attr rownum rowbody) =
      TableRow tablepart attr rownum [] rowbody

bodyRowsToJSX :: JSX a => [Ann.BodyRow] -> [a]
bodyRowsToJSX =
  map tableRowToJSX . zipWith toTableRow [1..]
  where
    toTableRow rownum (Ann.BodyRow attr _rownum rowhead rowbody) =
      TableRow Tbody attr rownum rowhead rowbody


tableRowToJSX :: JSX a => TableRow -> a
tableRowToJSX (TableRow tblpart attr _rownum rowhead rowbody) =
  simpleAttrJSXS "tr" attr (headcells ++ bodycells)
  where
    celltype = case tblpart of
                    Thead -> HeaderCell
                    _     -> BodyCell
    headcells = map (cellToJSX HeaderCell) rowhead
    bodycells = map (cellToJSX celltype) rowbody

cellToJSX :: JSX a => CellType -> Ann.Cell -> a
cellToJSX cellType (Ann.Cell _colspecs _colnum
                    (Cell attr align rowspan colspan item)) =
  buildJSXS (cellTag cellType)
  (cellAttr align rowspan colspan attr) $
  forceNoneEmpty (map writeJSXBlocks item)
  where forceNoneEmpty [] = [textJSX $ T.pack [chr 0xa0]]
        forceNoneEmpty x  = x

cellTag :: CellType -> Text
cellTag HeaderCell = "th"
cellTag BodyCell   = "td"

cellAttr :: Alignment -> RowSpan -> ColSpan -> Attr -> [JSXProp]
cellAttr align rowspan colspan attr =
  writeAttr attr ++ map mkProp (
  [ ("rowSpan", T.pack $ show (unRowSpan rowspan))
  , ("colSpan", T.pack $ show (unColSpan colspan))
  ] ++ maybe [] (\x -> [("style", x)]) (htmlAlignmentToString align))
  where
    unRowSpan (RowSpan n) = n
    unColSpan (ColSpan n) = n

    htmlAlignmentToString :: Alignment -> Maybe Text
    htmlAlignmentToString AlignLeft    = Just "{ textAlign: left; }"
    htmlAlignmentToString AlignRight   = Just "{ textAlign: right; }"
    htmlAlignmentToString AlignCenter  = Just "{ textAlign: center; }"
    htmlAlignmentToString AlignDefault = Nothing

    mkProp (k, v) = MapProp (writeDoubleQuotesJSString k, JSExpr v)
