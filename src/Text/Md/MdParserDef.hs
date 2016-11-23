{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParserDef
where

import           Text.Parsec (Parsec, ParsecT, Stream)
import qualified Data.Map as M

data Document = Document [Block] MetaData
  deriving (Show, Eq)

type RefId   = String
type RefLink  = String
data MetaData = MetaData { references :: M.Map RefId RefLink }
  deriving (Show, Eq)

-- TODO: list, blockquotes, codeblock
data Block = Header Int [Inline]
           | BlockHtml String
           | HorizontalRule
           | Paragraph [Inline]
           deriving (Show, Eq)

-- TODO: cite, link, code, (math)
data Inline = LineBreak
            | SoftBreak
            | Space
            | Strong [Inline]
            | InlineLink String String (Maybe String)
            | ReferenceLink String RefId
            | InlineHtml [Inline]
            | Str String
              deriving (Show, Eq)

class ReadMd a where
  parser :: Parsec String () a
  -- parser :: (Stream s m Char) => ParsecT s () Identity a

class WriteMd a where
  writeMd :: a -> String
