{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParser
where

import           Debug.Trace
import           System.IO
import           Text.Md.MdParserDef
import           Text.Md.ParseUtils
import           Text.Md.HtmlParser
import qualified Text.HTML.TagSoup as TS
import           Text.Parsec                   (Parsec, ParsecT, Stream, (<?>),
                                                (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try)

{-
-- TODO: escape of html symbols('<', '>', '&', '"')
data Document = Document [Block]
  deriving (Show, Eq)

-- TODO: list, blockquotes, codeblock, border
data Block = Header Int [Inline]
           | BlockHtml String
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
-}

instance ReadMd Document where
  parser = do blocks <- P.manyTill parser P.eof
              return $ Document blocks

instance ReadMd Block where
  parser = P.choice [ pHeader
                    , pHtmlBlock
                    , pParagraph
                    ]
           <?> "block"

{-
pHtmlBlock = P.try $ do
  blockElem <- pBlockElementDef
  skipSpaces
  blanklines
  return $ BlockHtml blockElem
-}
pHtmlBlock = P.try $ pBlockElementDef <* skipSpaces <* blanklines

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
                    , pInlineHtml
                    , pStr
                    , pEscape
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

pInlineHtml = P.try $ do
  let context = ParseContext parser :: ParseContext Inline
  pInlineElement context

-- (step 1) pBlockElementに内部のmarkdown処理を行うかどうかの選択肢を与える
-- (step 2) anyCharの前にhtml escapeを行う
pMark = P.try $ do
  P.notFollowedBy $ P.choice [spaceChar, blankline]
  let toStr = flip (:) []
  str <- toStr <$> P.try (P.char '\\' *> P.oneOf mdSymbols)
        <|> toStr <$> P.anyChar
  return $ Str str

-- TODO: escape '& ' but not like '&amp;'
pEscape = P.try (
  P.string    "<"  *> return (Str "&lt;")
 <|> P.string ">"  *> return (Str "&gt;")
 <|> P.string "&"  *> return (Str "&amp;")
 <|> P.string "\"" *> return (Str "&quot;")
 <?> "html-escape"
 )

mdSymbols = ['\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!']

{-
class WriteMd a where
  writeMd :: a -> String
-}

instance WriteMd Document where
  writeMd (Document blocks) = "<div>" ++ concatMap writeMd blocks ++ "</div>"

instance WriteMd Block where
  writeMd (Header level inlines) = "<h" ++ show level ++ ">" ++ concatMap writeMd inlines ++ "</h" ++ show level ++ ">"
  writeMd (BlockHtml str)        = str
  writeMd (Paragraph inlines)    = "<p>" ++ concatMap writeMd inlines ++ "</p>"

instance WriteMd Inline where
  writeMd LineBreak        = "<br />"
  writeMd SoftBreak        = " "
  writeMd Space            = " "
  writeMd (Strong inlines) = "<strong>" ++ concatMap writeMd inlines ++ "</strong>"
  writeMd (InlineHtml inlines) = concatMap writeMd inlines
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
