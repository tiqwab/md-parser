{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParserDef
where

import           Text.Parsec (Parsec, ParsecT, Stream)
import qualified Data.Map as M

data Document = Document [Block] MetaData
  deriving (Show, Eq)

data ParseContext = ParseContext { metadata :: MetaData }
  deriving (Show, Eq)

defContext = ParseContext { metadata = MetaData M.empty }

type RefId    = String
type RefLink  = String
type RefTitle = Maybe String
data MetaData = MetaData { references :: M.Map RefId (RefLink, RefTitle) }
  deriving (Show, Eq)

-- level, content. children
data ListItem = ListLineItem Int [Inline] [ListItem]
              | ListParaItem Int [Block] [ListItem]
  deriving (Show, Eq)

-- TODO: list, blockquotes
data Block = Header Int [Inline]
           | BlockHtml String
           | HorizontalRule
           | List ListItem
           | CodeBlock [Inline]
           | Paragraph [Inline]
           | NullB
           deriving (Show, Eq)

data Inline = LineBreak
            | SoftBreak
            | Space
            | Strong [Inline]
            | InlineLink String String (Maybe String)
            | ReferenceLink String RefId
            | InlineCode [Inline]
            | InlineHtml [Inline]
            | Str String
            | NullL
              deriving (Show, Eq)

class ReadMd a where
  parser :: Parsec String ParseContext a

class WriteMd a where
  writeMd :: a -> MetaData -> String
