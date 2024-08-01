-- | test if a Char is CJK
{-# LANGUAGE OverloadedStrings #-}


module Panda.CJK
  ( isCJK
  , isHalfFullForm
  , isRecoCJK
  , isAllRecoCJK
  , wrapCJK
  , isCJKSpan
  , removeCJKWhiteChar
  , processCJK
  )
where

import Data.Char (ord)
import Data.Text as T (Text, all)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)

-- | FIXME: only works for Chinese, try to including more
isCJK :: Char -> Bool
isCJK c = 0x4E00 <= code && code <= 0x9FFF || -- CJK Unified Ideographs blocks
  0x3400 <= code && code <= 0x4DBF ||         -- CJK Unified Ideographs Extension A
  0x20000 <= code && code <= 0x2A6DF ||       -- CJK Unified Ideographs Extension B
  0xFA00 <= code && code <= 0xFAFF ||         -- CJK Compatibility Ideographs
  0x3000 <= code && code <= 0x303F ||         -- CJK Symbols and Punctuation
  0x3040 <= code && code <= 0x309F ||         -- Hiragana
  0x30A0 <= code && code <= 0x30FF ||         -- Katakana
  0x31F0 <= code && code <= 0x31FF            -- Katakana Phonetic Extensions
  where code = ord c


-- | See [Halfwidth and Fullwidth Forms (Unicode block)](https://en.wikipedia.org/wiki/Halfwidth_and_Fullwidth_Forms_(Unicode_block))
isHalfFullForm :: Char -> Bool
isHalfFullForm c = 0xFF00 <= code && code <= 0xFFEF
  where code = ord c


-- | CJK or full-width chars
isRecoCJK :: Char -> Bool
isRecoCJK c = isCJK c || isHalfFullForm c


isAllRecoCJK :: Text -> Bool
isAllRecoCJK = T.all isRecoCJK

wrapCJK :: Inline -> Inline
wrapCJK (Str s) | isAllRecoCJK s = Span ("", ["cjk"], []) [Str s]
wrapCJK x = x

isCJKSpan :: Inline -> Bool
isCJKSpan (Span (_, l, _) _) = "cjk" `elem` l
isCJKSpan _ = False

removeCJKWhiteChar :: [Inline] -> [Inline]
removeCJKWhiteChar = reverse . go []
  where go acc (x:y:z:xs)
          | isWhite y && isCJKSpan x && isCJKSpan z = go (z:x:acc) xs
        go acc (x:xs) = go (x:acc) xs
        go acc [] = acc
        isWhite SoftBreak = True
        isWhite Space = True
        isWhite _ = False

processCJK :: [Block] -> [Block]
processCJK = walk removeCJKWhiteChar . walk wrapCJK
