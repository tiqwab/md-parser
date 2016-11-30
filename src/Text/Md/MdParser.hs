{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParser
where

import           Control.Monad
import           Debug.Trace
import           Data.Maybe (fromMaybe)
import           System.IO
import qualified Text.HTML.TagSoup             as TS
import           Text.Md.HtmlParser
import           Text.Md.MdParserDef
import           Text.Md.ParseUtils
import           Text.Parsec                   (Parsec, ParsecT, Stream, (<?>),
                                                (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try, runParser)
import qualified Data.Map as M

instance ReadMd Document where
  parser = do blocks <- P.manyTill parser P.eof
              meta   <- metadata <$> P.getState
              return $ Document blocks meta

instance ReadMd Block where
  parser = P.choice [ pHeader
                    , pHtmlBlock
                    , pHorizontalRule
                    , pReference
                    , pParagraph
                    ]
           <?> "block"

blanklineBetweenBlock  = blankline *> P.many (P.try blankline)
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

addRef refId (refLink, refTitle) state = state { metadata = newMeta }
  where oldMeta      = metadata state
        newMeta      = oldMeta { references = newRefs }
        originalRefs = references . metadata $ state
        newRefs      = M.insert refId (refLink, refTitle) originalRefs

-- TODO: Parse title
pReference = P.try $ do
  let pOneRef = do refId <- pEnclosed "[" "]"
                   P.char ':'
                   skipSpaces
                   refLink <- P.many1 (P.notFollowedBy blankline >> P.anyChar)
                   blankline
                   return (refId, refLink)
  refs <- P.many1 pOneRef
  blanklineBetweenBlock
  mapM_ (\(refId, refLink) -> P.updateState $ addRef refId (refLink, Nothing)) refs
  return Null

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
                    , pReferenceLink
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

pReferenceLink = P.try $ do
  text  <- pEnclosed "[" "]"
  P.optional spaceChar
  refId <- pEnclosed "[" "]"
  return $ ReferenceLink text refId

pEnclosed begin end = P.between pBegin pEnd (P.many1 pNones)
  where pBegin = P.string begin
        pEnd   = P.string end
        pNones = P.noneOf (begin ++ end)

pStr = P.try $ do
  -- str <- P.many1 (P.notFollowedBy pMark >> P.anyChar)
  str <- P.many1 P.alphaNum
  return $ Str str

pInlineHtml = P.try $ do
  let context = HtmlParseContext parser :: HtmlParseContext Inline
  pInlineElement context

pMark = P.try $ do
  P.notFollowedBy $ P.choice [spaceChar, blankline]
  let toStr = flip (:) []
  str <- toStr <$> P.try (P.char '\\' *> P.oneOf mdSymbols)
        <|> toStr <$> P.anyChar
  return $ Str str

mdSymbols = ['\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!']

instance WriteMd Document where
  writeMd (Document blocks meta) _ = "<div>" ++ concatMap (`writeMd` meta) blocks ++ "</div>"

instance WriteMd Block where
  writeMd (Header level inlines) meta = "<h" ++ show level ++ ">" ++ concatMap (`writeMd` meta) inlines ++ "</h" ++ show level ++ ">"
  writeMd (BlockHtml str) meta        = str
  writeMd HorizontalRule meta         = "<hr />"
  writeMd (Paragraph inlines) meta    = "<p>" ++ concatMap (`writeMd` meta) inlines ++ "</p>"
  writeMd Null meta                   = ""

hLink text link (Just title) = "<a href=\"" ++ link ++ "\" title=\"" ++ title ++ "\">" ++ text ++ "</a>"
hLink text link Nothing      = "<a href=\"" ++ link ++ "\">" ++ text ++ "</a>"

instance WriteMd Inline where
  writeMd LineBreak meta                           = "<br />"
  writeMd SoftBreak meta                           = " "
  writeMd Space meta                               = " "
  writeMd (Strong inlines) meta                    = "<strong>" ++ concatMap (`writeMd` meta) inlines ++ "</strong>"
  writeMd (ReferenceLink text linkId) meta         = case M.lookup linkId (references meta) of
                                                       Just (link, title) -> hLink text link title
                                                       Nothing            -> hLink text "" Nothing
  writeMd (InlineLink text link title) meta = hLink text link title
  writeMd (InlineHtml inlines) meta                = concatMap (`writeMd` meta) inlines
  writeMd (Str str) meta                           = str

readMarkdown :: String -> Document
readMarkdown input = case P.runParser parser defContext "" input of
                       Left  e -> error (show e) -- FIXME
                       Right s -> s

writeMarkdown :: Document -> String
writeMarkdown doc = writeMd doc (MetaData M.empty) -- FIXME

-- | Parse and convert markdown to html
parseMarkdown :: String -> String
parseMarkdown = writeMarkdown . readMarkdown
