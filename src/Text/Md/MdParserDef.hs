{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParserDef
where

import           Text.Parsec (Parsec, ParsecT, Stream)

data Document = Document [Block]
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
            | InlineHtml [Inline]
            | Str String
              deriving (Show, Eq)

class ReadMd a where
  parser :: Parsec String () a
  -- parser :: (Stream s m Char) => ParsecT s () Identity a

class WriteMd a where
  writeMd :: a -> String
