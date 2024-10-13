{-|
Module: Data.Text.Pangu
Description: Pangu text parsing and labeling
Copyright: (c) ayanamists
License: GPL-3
Maintainer: ayanamists@gmail.com
Stability: experimental

Inspired by [pangu.js](https://github.com/vinta/pangu.js), this module provides
functionality for parsing text and labeling segments based on character
categories. The main data type 'Pangu' represents a parsed text with labels, and
the 'PanguLabel' data type represents different categories of characters. The
'panguParse' function is the primary entry point for parsing a 'Text' into a
'Pangu'.

Functions included:

- 'panguParse': Parses a 'Text' into a 'Pangu' by labeling segments of the text.

Data types included:

- 'Pangu': Represents a parsed text with labels.
- 'PanguLabel': Represents different categories of characters.

Example usage:

@
import Data.Text.Pangu
import qualified Data.Text as T

-- Parse a text into a Pangu
parsedText :: Pangu
parsedText = panguParse $ T.pack "你好, world!"
@

-}

module Data.Text.Pangu
  ( Pangu(..)
  , PanguLabel(..)
  , panguParse
  ) where

import qualified Data.Text as T
import Data.Char (ord)
import Data.List (find)
import Data.Maybe (fromMaybe)

-- | A newtype wrapper around a list of labeled text segments.
newtype Pangu =
  -- | The constructor for 'Pangu'. It takes a list of labeled text segments.
  Pangu [(T.Text, PanguLabel)]
  deriving (Show)

-- | Represents different categories of characters.
data PanguLabel
  -- | Represents CJK (Chinese, Japanese, Korean) Unified Ideographs and related characters.
  = Han
  -- | Represents Hiragana characters.
  | Hiragana
  -- | Represents Katakana characters.
  | Katakana
  -- | Represents Latin characters.
  | Latin
  -- | Represents Full-width and Half-width forms of characters.
  | FullWidthForm
  -- | Represents any other characters not covered by the above categories.
  | Other
  deriving (Show, Eq)

data Range a l = Range a a l
type Ranges a l = [Range a l]

findLabel :: Ord a => Ranges a l -> l -> a -> l
findLabel rs l v = fromMaybe l $ getLabel <$> find (inRange v) rs
  where inRange v' (Range st ed _) = st <= v' && v' <= ed
        getLabel (Range _ _ l') = l'

mkRanges :: [(a, a)] -> l -> Ranges a l
mkRanges rs l = map (\x -> Range (fst x) (snd x) l) rs

panguLabelRange :: Ranges Int PanguLabel
panguLabelRange = concat
  [ mkRanges [ (0x4E00, 0x9FFF)     -- CJK Unified Ideographs blocks
             , (0x3400, 0x4DBF)     -- CJK Unified Ideographs Extension A
             , (0x20000, 0x2A6DF)   -- CJK Unified Ideographs Extension B
             , (0xF900, 0xFAFF)     -- CJK Compatibility Ideographs
             , (0x3000, 0x303F)     -- CJK Symbols and Punctuation
             ] Han
  , mkRanges [ (0x3040, 0x309F) ] Hiragana
  , mkRanges [ (0x30A0, 0x30FF)
             , (0x31F0, 0x31FF)     -- Katakana Phonetic Extensions
             ] Katakana
  , mkRanges [ (0x0041, 0x005A)     -- Latin Uppercase Letters
             , (0x0061, 0x007A)     -- Latin Lowercase Letters
             , (0x00C0, 0x00D6)     -- Latin Extended-A Uppercase
             , (0x00D8, 0x00F6)     -- Latin Extended-A Lowercase
             , (0x00F8, 0x00FF)     -- Latin Extended-A Additional
             , (0x0100, 0x017F)     -- Latin Extended-A
             , (0x0180, 0x024F)     -- Latin Extended-B
             ] Latin
  , mkRanges [ (0xFF00, 0xFFEF)
             ] FullWidthForm        -- Halfwidth and Fullwidth Forms
  ]

toPanguLabel :: Char -> PanguLabel
toPanguLabel = findLabel panguLabelRange Other . ord

-- | 'panguParse' parses a 'Text' into a 'Pangu'.
-- It segments the text into parts based on their 'PanguLabel'.
panguParse :: T.Text -> Pangu
panguParse = Pangu . map packs . foldr join [] . map annotateLabel . T.unpack
  where annotateLabel c = (c, toPanguLabel c)
        packs (s, l) = (T.pack s, l)
        join (c, cl) [] = [([c], cl)]
        join (c, cl) segs@((s, l):r)
          | cl == l   = (c:s, l):r
          | otherwise = ([c], cl):segs
