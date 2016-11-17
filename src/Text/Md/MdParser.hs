{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParser
where

import           Control.Monad
import           Debug.Trace
import           System.IO
import qualified Text.HTML.TagSoup             as TS
import           Text.Md.HtmlParser
import           Text.Md.MdParserDef
import           Text.Md.ParseUtils
import           Text.Parsec                   (Parsec, ParsecT, Stream, (<?>),
                                                (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try)

instance ReadMd Document where
  parser = do blocks <- P.manyTill parser P.eof
              return $ Document blocks

instance ReadMd Block where
  parser = P.choice [ pHeader
                    , pHtmlBlock
                    , pHorizontalRule
                    , pParagraph
                    ]
           <?> "block"

blanklinesBetweenBlock = blankline *> blankline *> P.many (P.try blankline)

pHeader = P.try $ do
  level <- length <$> P.many1 (P.char '#')
  skipSpaces
  inlines <- P.many1 (P.notFollowedBy blanklines >> parser)
  blanklinesBetweenBlock
  return $ Header level inlines

pHtmlBlock = P.try $ pBlockElementDef <* skipSpaces <* blanklinesBetweenBlock

pHorizontalRule = P.try $ P.choice [pBorder '-', pBorder '*', pBorder '_']

pBorder char = P.try $ do
  chars <- P.many1 (P.char char <* skipSpaces)
  blanklinesBetweenBlock
  guard $ length chars >= 3
  return HorizontalRule

pParagraph = P.try $ do
  -- inlines <- P.many1 (P.notFollowedBy blanklines >> parser)
  inlines <- P.many1 parser
  blanklinesBetweenBlock
  return $ Paragraph inlines

instance ReadMd Inline where
  parser = P.choice [ pLineBreak
                    , pSoftBreak
                    , pSpace
                    , pStrong
                    , pInlineLink
                    , pInlineHtml
                    , pStr
                    , pHtmlEscape
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

pInlineLink = P.try $ do
  let pText         = P.many1 (P.notFollowedBy (P.char ']') >> P.anyChar)
      pLinkAndTitle = do text <- P.many1 (P.notFollowedBy (P.oneOf " )") >> P.anyChar)
                         skipSpaces
                         title <- P.optionMaybe $ P.char '"' *> P.many (P.notFollowedBy (P.char '"') >> P.anyChar) <* P.char '"'
                         return (text, title)
  text          <- P.between (P.char '[') (P.char ']') pText
  (link, title) <- P.between (P.char '(') (P.char ')') pLinkAndTitle
  return $ InlineLink text link title

pStr = P.try $ do
  -- str <- P.many1 (P.notFollowedBy pMark >> P.anyChar)
  str <- P.many1 P.alphaNum
  return $ Str str

pInlineHtml = P.try $ do
  let context = ParseContext parser :: ParseContext Inline
  pInlineElement context

pMark = P.try $ do
  P.notFollowedBy $ P.choice [spaceChar, blankline]
  let toStr = flip (:) []
  str <- toStr <$> P.try (P.char '\\' *> P.oneOf mdSymbols)
        <|> toStr <$> P.anyChar
  return $ Str str

mdSymbols = ['\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!']

instance WriteMd Document where
  writeMd (Document blocks) = "<div>" ++ concatMap writeMd blocks ++ "</div>"

instance WriteMd Block where
  writeMd (Header level inlines) = "<h" ++ show level ++ ">" ++ concatMap writeMd inlines ++ "</h" ++ show level ++ ">"
  writeMd (BlockHtml str)        = str
  writeMd HorizontalRule         = "<hr />"
  writeMd (Paragraph inlines)    = "<p>" ++ concatMap writeMd inlines ++ "</p>"

instance WriteMd Inline where
  writeMd LineBreak        = "<br />"
  writeMd SoftBreak        = " "
  writeMd Space            = " "
  writeMd (Strong inlines) = "<strong>" ++ concatMap writeMd inlines ++ "</strong>"
  writeMd (InlineLink text link (Just title)) = "<a href=\"" ++ link ++ "\" title=\"" ++ title ++ "\">" ++ text ++ "</a>"
  writeMd (InlineLink text link Nothing)      = "<a href=\"" ++ link ++ "\">" ++ text ++ "</a>"
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
