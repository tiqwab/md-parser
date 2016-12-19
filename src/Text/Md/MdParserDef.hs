{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParserDef (
    Document (..)
  , ParseContext (..)
  , defContext
  , MetaData (..)
  , defMetaData
  , ListItem (..)
  , Block (..)
  , Inline (..)
  , ReadMarkDown (..)
  , WriteMarkDown (..)
)
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
                                 , quoteLevel :: Int -- depth of the current quote block
                                 }
  deriving (Show, Eq)

defContext = ParseContext { metadata = defMetaData
                          , lineStart = '\0'
                          , isLastNewLineQuoted = False
                          , quoteLevel = 0
                          }

type RefId    = String
type RefLink  = String
type RefTitle = Maybe String
data MetaData = MetaData { references :: M.Map RefId (RefLink, RefTitle) -- info of reference links
                         , doesFormatHtml :: Bool -- should format a converted html?
                         }
  deriving (Show, Eq)

defMetaData = MetaData { references = M.empty
                       , doesFormatHtml = False
                       }

-- | Tree node of List. List block consists of ListItems.
-- level, content. children
data ListItem = ListLineItem Int [Inline] [ListItem]
              | ListParaItem Int [Block] [ListItem]
  deriving (Show, Eq)

-- | Data representing html block elements
data Block = Header Int [Inline]
           | BlockHtml [Inline]
           | HorizontalRule
           | List ListItem
           | CodeBlock [Inline]
           | BlockQuote [Block]
           | Paragraph [Inline]
           | NullB
           deriving (Show, Eq)

-- | Data representing html inline elements
data Inline = LineBreak
            | SoftBreak
            | Space
            | Strong [Inline]
            | Emphasis [Inline]
            | InlineLink String String (Maybe String)
            | ReferenceLink String RefId
            | InlineCode [Inline]
            | InlineHtml [Inline]
            | Str String
            | NullL
              deriving (Show, Eq)

-- | Can be read as markdown
class ReadMarkDown a where
  parser :: Parsec String ParseContext a

-- | Can be written to html codes
class WriteMarkDown a where
  writeMarkDown :: a -> MetaData -> String
