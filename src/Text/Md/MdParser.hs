{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParser
where

import           Debug.Trace
import           System.IO
import           Text.Md.ParseUtils
import           Text.Md.HtmlParser
import qualified Text.HTML.TagSoup as TS
import           Text.Parsec                   (Parsec, ParsecT, Stream, (<?>),
                                                (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try)

-- TODO: escape of html symbols('<', '>', '&', '"')
data Document = Document [Block]
  deriving (Show, Eq)

-- TODO: list, blockquotes, codeblock, border
data Block = Header Int [Inline]
           | HtmlBlock String
           | Paragraph [Inline]
           deriving (Show, Eq)

-- TODO: cite, link, code, (math)
data Inline = LineBreak
            | SoftBreak
            | Space
            | Strong [Inline]
            | Str String
            deriving (Show, Eq)

class ReadMd a where
  parser :: Parsec String () a
  -- parser :: (Stream s m Char) => ParsecT s () Identity a

instance ReadMd Document where
  parser = do blocks <- P.manyTill parser P.eof
              return $ Document blocks

instance ReadMd Block where
  parser = P.choice [ pHeader
                    , pHtmlBlock
                    , pParagraph
                    ]
           <?> "block"

pHtmlBlock = P.try $ do
  blockElem <- pBlockElement
  skipSpaces
  blanklines
  return $ HtmlBlock blockElem

pHeader = P.try $ do
  level <- length <$> P.many1 (P.char '#')
  skipSpaces
  inlines <- P.many1 (P.notFollowedBy blanklines >> parser)
  blanklines
  return $ Header level inlines

pParagraph = P.try $ do
  -- inlines <- P.many1 (P.notFollowedBy blanklines >> parser)
  inlines <- P.many1 parser
  blanklines
  return $ Paragraph inlines

instance ReadMd Inline where
  parser = P.choice [ pLineBreak
                    , pSoftBreak
                    , pSpace
                    , pStrong
                    , pStr
                    , pMark
                    ]
           <?> "inline"

pLineBreak = P.try $ do
  P.count 2 (P.char ' ') >> blankline
  return LineBreak

pSoftBreak = P.try $ do
  blankline >> P.notFollowedBy blankline
  return SoftBreak

pSpace = P.try $ do
  spaceChar >> skipSpaces
  return Space

pStrong = P.try $ do
  P.string "**"
  -- inlines <- P.many1 parser
  inlines <- P.many1 (P.notFollowedBy (P.string "**") >> parser)
  P.string "**"
  return $ Strong inlines

pStr = P.try $ do
  -- str <- P.many1 (P.notFollowedBy pMark >> P.anyChar)
  str <- P.many1 P.alphaNum
  return $ Str str

pMark = P.try $ do
  P.notFollowedBy $ P.choice [spaceChar, blankline]
  c <- P.try (P.char '\\' *> P.oneOf mdSymbols)
      <|> P.anyChar
  return $ Str [c]

mdSymbols = ['\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!']

class WriteMd a where
  writeMd :: a -> String

instance WriteMd Document where
  writeMd (Document blocks) = "<div>" ++ concatMap writeMd blocks ++ "</div>"

instance WriteMd Block where
  writeMd (Header level inlines) = "<h" ++ show level ++ ">" ++ concatMap writeMd inlines ++ "</h" ++ show level ++ ">"
  writeMd (HtmlBlock str)        = str
  writeMd (Paragraph inlines)    = "<p>" ++ concatMap writeMd inlines ++ "</p>"

instance WriteMd Inline where
  writeMd LineBreak        = "<br />"
  writeMd SoftBreak        = " "
  writeMd Space            = " "
  writeMd (Strong inlines) = "<strong>" ++ concatMap writeMd inlines ++ "</strong>"
  writeMd (Str str)        = str

readMarkdown :: String -> Document
readMarkdown input = case P.parse parser "" input of
                       Left  e -> error (show e) -- FIX ME
                       Right s -> s

writeMarkdown :: Document -> String
writeMarkdown doc = writeMd doc

-- | Parse and convert markdown to html
parseMarkdown :: String -> String
parseMarkdown = writeMarkdown . readMarkdown
