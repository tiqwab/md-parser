{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParserDef
where

import           Text.Parsec (Parsec, ParsecT, Stream)
import qualified Data.Map as M

-- | Intermediate data structure between markdown and html
data Document = Document [Block] MetaData
  deriving (Show, Eq)

-- | Parser state used in the conversion of markdown to html
data ParseContext = ParseContext { metadata :: MetaData -- meta data of document
                                 , lineStart :: Char -- character at the start of line
                                 , isLastNewLineQuoted :: Bool -- last newline is quoted?
                                 }
  deriving (Show, Eq)

defContext = ParseContext { metadata = MetaData M.empty
                          , lineStart = '\0'
                          , isLastNewLineQuoted = False
                          }

type RefId    = String
type RefLink  = String
type RefTitle = Maybe String
data MetaData = MetaData { references :: M.Map RefId (RefLink, RefTitle) -- info of reference links
                         }
  deriving (Show, Eq)

-- | Tree node of List. List block consists of ListItems.
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
           | BlockQuote [Block]
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

-- | Can be read as markdown
class ReadMd a where
  parser :: Parsec String ParseContext a

-- | Can be written to html codes
class WriteMd a where
  writeMd :: a -> MetaData -> String
