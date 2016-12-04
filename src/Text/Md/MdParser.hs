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
import           Text.Md.HtmlTags
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
                    , pListBlock
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

pListBlock = P.try $ P.choice [pList '-', pList '*', pList '+']

pListIndent = P.try (P.count 4 (P.char ' ')) <|> P.string "\t"

pList char = P.try $ do
  let pItem =  P.try (pListLineItem char <* P.notFollowedBy (P.try (blankline *> P.string [char]) <|> (blankline *> pListIndent)))
           <|> pListParaItem char
  firstItem <- pItem
  items <- case firstItem of
    ListLineItem _ -> P.many (pListLineItem char <* P.notFollowedBy (blankline *> P.char char))
    ListParaItem _ -> P.many (pListParaItem char)
  P.optional blankline
  return $ List (firstItem:items)

-- FIXME: Should ignore soft break or first spaces the following lines in paragraphs
pListLineItem char = P.try $ do
  P.char char
  P.many1 spaceChar
  inlines <- P.many1 (P.notFollowedBy (blankline *> P.char char) >> parser)
  blankline
  return $ ListLineItem inlines

-- FIXME: Should ignore soft break or first spaces the following lines in paragraphs
pListParaItem char = P.try $ do
  P.char char
  P.many1 spaceChar
  content <- P.sepBy (P.notFollowedBy (P.char char) >> pParagraph) pListIndent
  return $ ListParaItem content

addRef (refId, refLink, refTitle) state = state { metadata = newMeta }
  where oldMeta      = metadata state
        newMeta      = oldMeta { references = newRefs }
        originalRefs = references . metadata $ state
        newRefs      = M.insert refId (refLink, refTitle) originalRefs

-- TODO: Parse title
pReference = P.try $ do
  let pOneRef = do refId <- pEnclosed "[" "]"
                   P.char ':'
                   skipSpaces
                   refLink <- P.many1 (P.notFollowedBy (spaceChar <|> blankline) >> P.anyChar)
                   refTitle <- P.optionMaybe $ skipSpaces *> pEnclosed "\"" "\""
                   blankline
                   return (refId, refLink, refTitle)
  refs <- P.many1 pOneRef
  blanklineBetweenBlock
  mapM_ (P.updateState . addRef) refs
  return NullB

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
                    , pInlineCode
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

pEnclosed begin end = pEnclosedP begin end (P.many1 pNones)
  where pNones = P.noneOf (begin ++ end)

pEnclosedP begin end = P.between pBegin pEnd
  where pBegin = P.string begin
        pEnd   = P.string end

pStr = P.try $ do
  -- str <- P.many1 (P.notFollowedBy pMark >> P.anyChar)
  str <- P.many1 P.alphaNum
  return $ Str str

pInlineCode = P.try $ do
  let pSingleStr = P.anyChar >>= (\x -> return $ Str [x])
  start <- P.many1 $ P.try (P.char '`')
  codes <- P.manyTill (P.try pHtmlEscape <|> pSingleStr) (P.try (P.string start))
  return $ InlineCode codes

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
  writeMd (List items) meta           = hList $ map writeListLine items
    where writeListLine (ListLineItem inlines) = concatMap (`writeMd` meta) inlines
          writeListLine (ListParaItem paras)   = concatMap (`writeMd` meta) paras
  writeMd HorizontalRule meta         = "<hr />"
  writeMd (Paragraph inlines) meta    = "<p>" ++ concatMap (`writeMd` meta) inlines ++ "</p>"
  writeMd NullB meta                  = ""

instance WriteMd Inline where
  writeMd LineBreak meta                           = "<br />"
  writeMd SoftBreak meta                           = " "
  writeMd Space meta                               = " "
  writeMd (Strong inlines) meta                    = "<strong>" ++ concatMap (`writeMd` meta) inlines ++ "</strong>"
  writeMd (ReferenceLink text linkId) meta         = case M.lookup linkId (references meta) of
                                                       Just (link, title) -> hLink text link title
                                                       Nothing            -> hLink text "" Nothing
  writeMd (InlineLink text link title) meta = hLink text link title
  writeMd (InlineCode inlines) meta                = let text = trim $ concatMap (`writeMd` meta) inlines
                                                     in "<code>" ++ text ++ "</code>"
  writeMd (InlineHtml inlines) meta                = concatMap (`writeMd` meta) inlines
  writeMd (Str str) meta                           = str
  writeMd NullL meta                               = ""

readMarkdown :: String -> Document
readMarkdown input = case P.runParser parser defContext "" input of
                       Left  e -> error (show e) -- FIXME
                       Right s -> s

writeMarkdown :: Document -> String
writeMarkdown doc = writeMd doc (MetaData M.empty) -- FIXME

-- | Parse and convert markdown to html
parseMarkdown :: String -> String
parseMarkdown = writeMarkdown . readMarkdown
