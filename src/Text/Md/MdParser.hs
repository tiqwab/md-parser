{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParser
where

import           Control.Monad
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe)
import           Debug.Trace
import           System.IO
import qualified Text.HTML.TagSoup             as TS
import           Text.Md.HtmlParser
import           Text.Md.HtmlTags
import           Text.Md.MdParserDef
import           Text.Md.ParseUtils
import           Text.Parsec                   (Parsec, ParsecT, Stream, (<?>),
                                                (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (runParser, try)

instance ReadMd Document where
  parser = do blocks <- P.manyTill parser P.eof
              meta   <- metadata <$> P.getState
              return $ Document blocks meta

{-
Block elements
-}

instance ReadMd Block where
  parser = P.choice [ pHeader
                    , pHtmlBlock
                    , pHorizontalRule
                    , pListBlock
                    , pReference
                    , pCodeBlock
                    , pBlockQuote
                    , pParagraph
                    ]
           <?> "block"

-- | Parse newlines between blocks. Expect more than one newlines.
blanklineBetweenBlock  = blankline *> P.many (P.try blankline)
-- | Parse newlines between blocks. Expect more than two newlines.
blanklinesBetweenBlock = blankline *> blankline *> P.many (P.try blankline)

----- Header -----

-- | Parse a header.
-- Accept only the 'Atx' form without closing hashes, not the 'Setext' form.
pHeader = P.try $ do
  level <- length <$> P.many1 (P.char '#')
  skipSpaces
  inlines <- P.many1 (P.notFollowedBy blanklines >> parser)
  blanklinesBetweenBlock
  return $ Header level inlines

----- Header -----

----- Html Block -----

-- | Parse a html block. Text inside html tags are escaped, but markdown literals are not processed.
pHtmlBlock = P.try $ do
  let htmlParseContext = HtmlParseContext pStrWithHtmlEscape
  pBlockElement htmlParseContext <* skipSpaces <* blanklinesBetweenBlock

----- Html Block -----

----- Horizontal rule -----

-- | Parse a horizontal rule. Accept three kinds of symbols ('-', '*', and '_').
pHorizontalRule = P.try $ P.choice [pBorder '-', pBorder '*', pBorder '_']

pBorder char = P.try $ do
  chars <- P.many1 (P.char char <* skipSpaces)
  blanklinesBetweenBlock
  guard $ length chars >= 3
  return HorizontalRule

----- Horizontal rule -----

----- List -----

-- | Parse a list. Three kinds of symbols ('-', '*', and '+') can be handled as items of unordered lists.
-- List items can be converted to paragraphs if each one is separated by blanklines.
-- An item can consist of multiple paragraphs.
pListBlock = P.try $ P.choice [pList '-', pList '*', pList '+']

pListIndent = P.try (P.count 4 (P.char ' ')) <|> P.string "\t"

insertL 0     val node@(ListLineItem l v cs) = ListLineItem l v (cs ++ [ ListLineItem (l+1) val [] ])
insertL level val node@(ListLineItem l v cs) = ListLineItem l v newCs
  where (formers, lastC) = splitAt (length cs - 1) cs
        newCs            = formers ++ fmap (insertL (level-1) val) lastC

insertP 0     val node@(ListParaItem l v cs) = ListParaItem l v (cs ++ [ ListParaItem (l+1) val [] ])
insertP level val node@(ListParaItem l v cs) = ListParaItem l v newCs
  where (formers, lastC) = splitAt (length cs - 1) cs
        newCs            = formers ++ fmap (insertP (level-1) val) lastC

toListL items = foldl summarize (ListLineItem 0 [NullL] []) items
  where summarize node (ListLineItem l v cs) = insertL (l-1) v node

toListP items = foldl summarize (ListParaItem 0 [NullB] []) items
  where summarize node (ListParaItem l v cs) = insertP (l-1) v node

-- TODO: summarize L and P
pList char = P.try $ do
  let pItem =  P.try (pListLineItem char <* P.notFollowedBy (P.try (blankline *> P.string [char]) <|> (blankline *> P.many1 spaceChar)))
           <|> pListParaItem char
  firstItem <- pItem
  case firstItem of
    ListLineItem {} -> do items <- P.many (pListLineItem char <* P.notFollowedBy (blankline *> P.char char))
                          P.optional blankline
                          return $ List (toListL (firstItem:items))
    ListParaItem {} -> do items <- P.many (pListParaItem char)
                          P.optional blankline
                          return $ List (toListP (firstItem:items))

pListLineItem char = P.try $ do
  indents <- P.many pListIndent
  P.char char
  spaceSep <- P.many1 (P.char ' ')
  guard $ length spaceSep <= 3
  inlines <- P.many1 (P.notFollowedBy (blankline *> P.optional (P.many pListIndent) *> P.char char) >> parser)
  blankline
  return $ ListLineItem (length indents + 1) inlines []

pListParaItem char = P.try $ do
  indents <- P.many pListIndent
  P.char char
  spaceSep <- P.many1 (P.char ' ')
  guard $ length spaceSep <= 3
  let pIndent   = P.count (length indents) pListIndent
      pSpaceSep = P.count (length spaceSep + 1) (P.char ' ')
  firstContent <- pParagraph
  followingContent <- P.many (P.notFollowedBy (P.many pListIndent *> P.char char) *> pIndent *> pSpaceSep *> P.notFollowedBy (P.char char) *> pParagraph)
  return $ ListParaItem (length indents + 1) (firstContent : followingContent) []

----- List -----

----- Reference -----

addRef (refId, refLink, refTitle) state = state { metadata = newMeta }
  where oldMeta      = metadata state
        newMeta      = oldMeta { references = newRefs }
        originalRefs = references . metadata $ state
        newRefs      = M.insert refId (refLink, refTitle) originalRefs

-- | Parse a pair or label and link for reference links.
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

----- Reference -----

----- Code block -----

-- | Parse a code block. Escape any '<', '>', '"', '&' characters inside blocks. FIXME
pCodeBlock = P.try $ do
  P.string "```" >> newlineQuote
  xs <- P.many (P.notFollowedBy (newlineQuote >> P.string "```") >> pStrWithHtmlEscape)
  newlineQuote >> P.string "```"
  blanklinesBetweenBlock
  return $ CodeBlock xs

----- Code block -----

----- Block quote -----

-- | Parse a blockquote. Blockquotes can contain other kinds of blocks including blockquotes.
pBlockQuote = P.try $ do
  let plusQuoteLevel context = context { quoteLevel = quoteLevel context + 1 }
      minusQuoteLevel context = context { quoteLevel = quoteLevel context - 1 }
  let updateLineStart c context = context { lineStart = c }
  let pFollowingBlock = do b <- isLastNewLineQuoted <$> P.getState
                           guard b
                           parser
  P.char '>' >> skipSpaces
  originalChar <- lineStart <$> P.getState
  P.modifyState plusQuoteLevel
  P.modifyState (updateLineStart '>')
  firstBlock <- parser
  followingBlocks <- P.many pFollowingBlock
  P.modifyState minusQuoteLevel
  P.modifyState (updateLineStart originalChar)
  level <- quoteLevel <$> P.getState
  when (level > 0) (P.modifyState (\context -> context { isLastNewLineQuoted = True }))
  return $ BlockQuote (firstBlock : followingBlocks)

----- Block quote -----

----- Paragraph -----

-- | Parse a paragraph.
-- All blocks which are not parsed as other kinds of blocks will be handled as paragraphs.
pParagraph = P.try $ do
  inlines <- P.many1 parser
  blanklinesBetweenBlock
  return $ Paragraph inlines

----- Paragraph -----

{-
Inline elements
-}

instance ReadMd Inline where
  parser = P.choice [ pLineBreak
                    , pSoftBreak
                    , pSpace
                    , pStrong
                    , pEmphasis
                    , pInlineLink
                    , pInlineCode
                    , pInlineHtml
                    , pReferenceLink
                    , pStr
                    , pHtmlEscape
                    , pMark
                    ]
           <?> "inline"

----- Line break -----

-- | Parse more than two spaces at the end of line.
pLineBreak = P.try $ do
  P.count 2 (P.char ' ') >> blankline
  return LineBreak

----- Line break -----

----- Soft break -----

-- | Parse soft break('\n')
pSoftBreak = P.try $ do
  blankline >> skipSpaces >> P.notFollowedBy P.newline
  return SoftBreak

----- Soft break -----

----- Space -----

-- | Parse more than one spaces and treats as only one space.
pSpace = P.try $ do
  spaceChar >> skipSpaces
  return Space

----- Space -----

----- Strong and emphasis -----

-- | Parse a strong framed by two '*' or '_'.
pStrong = P.try (P.choice [parserStrong "**", parserStrong "__"])
  where parserStrong s = Strong <$> pEnclosedInline s s

-- | Parse a emphasis framed by one '*' or '_'.
pEmphasis = P.try (P.choice [parserEmphasis "*", parserEmphasis "_"])
  where parserEmphasis s = Emphasis <$> pEnclosedInline s s

----- Strong and emphasis -----

----- Link -----

-- | Parse a inline link such as '[foo](https://foobar.com  "foo")'.
pInlineLink = P.try $ do
  let pText         = P.many1 (P.notFollowedBy (P.char ']') >> P.anyChar)
      pLinkAndTitle = do text <- P.many1 (P.notFollowedBy (P.oneOf " )") >> P.anyChar)
                         skipSpaces
                         title <- P.optionMaybe $ P.char '"' *> P.many (P.notFollowedBy (P.char '"') >> P.anyChar) <* P.char '"'
                         return (text, title)
  text          <- P.between (P.char '[') (P.char ']') pText
  (link, title) <- P.between (P.char '(') (P.char ')') pLinkAndTitle
  return $ InlineLink text link title

-- | Parse a reference link such as '[foo][1]'.
-- There must be a corresponding link with the label anywhere in the document.
pReferenceLink = P.try $ do
  text  <- pEnclosed "[" "]"
  P.optional spaceChar
  refId <- pEnclosed "[" "]"
  return $ ReferenceLink text refId

pEnclosed begin end = pEnclosedP begin end pNones
  where pNones = P.noneOf (begin ++ end)

pEnclosedInline begin end = pEnclosedP begin end parser

pEnclosedP begin end parser = do
  P.string begin
  ps <- P.many1 (P.notFollowedBy (P.string end) >> parser)
  P.string end
  return ps

----- Link -----

----- Str -----

-- | Parse a string consisting of any alphabets and digits.
pStr = P.try $ do
  str <- P.many1 P.alphaNum
  return $ Str str

-- | Parse a string. Accept any marks as well as alphanums and perform html-escaping.
pStrWithHtmlEscape = P.try pHtmlEscape <|> P.try pStr <|> P.try pNewlineQuote <|> pSingleStr
  where pNewlineQuote = newlineQuote >>= (\x -> return $ Str [x])
        pSingleStr    = P.anyChar >>= (\x -> return $ Str [x])

----- Str -----

----- Inline code -----

-- | Parse a inline code.
pInlineCode = P.try $ do
  start <- P.many1 $ P.try (P.char '`')
  codes <- P.manyTill pStrWithHtmlEscape (P.try (P.string start))
  return $ InlineCode codes

----- Inline code -----

----- Inline html -----

-- | Parse inline html. Inline markdown literals and html escaping are active.
pInlineHtml = P.try $ do
  let context = HtmlParseContext parser :: HtmlParseContext Inline
  pInlineElement context

----- Inline html -----

----- Str with marks -----

-- | Parse a mark. Escape markdown literals if necssary.
pMark = P.try $ do
  P.notFollowedBy $ P.choice [spaceChar, blankline]
  let toStr = flip (:) []
  str <- toStr <$> P.try (P.char '\\' *> P.oneOf mdSymbols)
        <|> toStr <$> P.anyChar
  return $ Str str

mdSymbols = ['\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!']

----- Str with marks -----

{-
Implementation of WriteMd
-}

instance WriteMd Document where
  writeMd (Document blocks meta) _ = "<div>" ++ concatMap (`writeMd` meta) blocks ++ "</div>"

instance WriteMd Block where
  writeMd (Header level inlines) meta = "<h" ++ show level ++ ">" ++ concatMap (`writeMd` meta) inlines ++ "</h" ++ show level ++ ">"
  writeMd (BlockHtml inlines) meta        = concatMap (`writeMd` meta) inlines
  writeMd (List item@(ListLineItem {})) meta = toStrL item
    where toStrL node = toLinesL node
          toLinesL node@(ListLineItem 0 _ cs) = "<ul>" ++ concatMap toLinesL cs ++ "</ul>"
          toLinesL node@(ListLineItem l v []) = "<li>" ++ toLineL node ++ "</li>"
          toLinesL node@(ListLineItem l v cs) = "<li>" ++ toLineL node ++ "<ul>" ++ concatMap toLinesL cs ++ "</ul>" ++ "</li>"
          toLineL  node@(ListLineItem l v cs) = concatMap (`writeMd` meta) v
  writeMd (List item@(ListParaItem {})) meta = toStrP item
    where toStrP node = toLinesP node
          toLinesP node@(ListParaItem 0 _ cs) = "<ul>" ++ concatMap toLinesP cs ++ "</ul>"
          toLinesP node@(ListParaItem l v []) = "<li>" ++ toLineP node ++ "</li>"
          toLinesP node@(ListParaItem l v cs) = "<li>" ++ toLineP node ++ "<ul>" ++ concatMap toLinesP cs ++ "</ul>" ++ "</li>"
          toLineP  node@(ListParaItem l v cs) = concatMap (`writeMd` meta) v
  writeMd HorizontalRule meta         = "<hr />"
  writeMd (CodeBlock inlines) meta    = "<pre><code>" ++ concatMap (`writeMd` meta) inlines ++ "</code></pre>"
  writeMd (BlockQuote blocks) meta    = "<blockquote>" ++ concatMap (`writeMd` meta) blocks ++ "</blockquote>"
  writeMd (Paragraph inlines) meta    = "<p>" ++ concatMap (`writeMd` meta) inlines ++ "</p>"
  writeMd NullB meta                  = ""

instance WriteMd Inline where
  writeMd LineBreak meta                           = "<br />"
  writeMd SoftBreak meta                           = " "
  writeMd Space meta                               = " "
  writeMd (Strong inlines) meta                    = "<strong>" ++ concatMap (`writeMd` meta) inlines ++ "</strong>"
  writeMd (Emphasis inlines) meta                  = "<em>" ++ concatMap (`writeMd` meta) inlines ++ "</em>"
  writeMd (ReferenceLink text linkId) meta         = case M.lookup linkId (references meta) of
                                                       Just (link, title) -> hLink text link title
                                                       Nothing            -> hLink text "" Nothing
  writeMd (InlineLink text link title) meta = hLink text link title
  writeMd (InlineCode inlines) meta                = let text = trim $ concatMap (`writeMd` meta) inlines
                                                     in "<code>" ++ text ++ "</code>"
  writeMd (InlineHtml inlines) meta                = concatMap (`writeMd` meta) inlines
  writeMd (Str str) meta                           = str
  writeMd NullL meta                               = ""

{-
functions to handle conversion of markdown
-}

-- | Convert markdown to document.
readMarkdown :: String -> Document
readMarkdown input = case P.runParser parser defContext "" input of
                       Left  e -> error (show e)
                       Right s -> s

-- | Convert document to html.
writeMarkdown :: Document -> String
writeMarkdown doc = writeMd doc (MetaData M.empty)

-- | Parse and convert markdown to html.
parseMarkdown :: String -> String
parseMarkdown = writeMarkdown . readMarkdown
