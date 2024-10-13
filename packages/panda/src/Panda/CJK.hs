-- | Processing CJK, wrap them to <span class="cjk">...</span>
{-# LANGUAGE OverloadedStrings #-}


module Panda.CJK
  ( processCJK
  )
where

import qualified Data.Text as T
import Data.Text.Pangu
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk)

unPangu :: Pangu -> [(T.Text, PanguLabel)]
unPangu (Pangu l) = l

wrapSpanCJK :: T.Text -> Inline
wrapSpanCJK t = Span ("", ["cjk"], []) [Str t]

wrapCJK :: [Inline] -> [Inline]
wrapCJK = concatMap processStr

processStr :: Inline -> [Inline]
processStr (Str s) = map wrap $ unPangu $ panguParse s
  where wrap (t, l)
          | l `elem` [Han, Katakana, Hiragana, FullWidthForm] = wrapSpanCJK t
          | otherwise = Str t
processStr x = [x]

processCJK :: [Block] -> [Block]
processCJK = walk wrapCJK
