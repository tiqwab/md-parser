{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.HtmlParser (
    HtmlParseContext(..)
  , pBlockElementDef
  , pBlockElement
  , pInlineElement
  , pHtmlEscape
)
where

import           Control.Monad
import           Debug.Trace
import           System.IO
import qualified Text.HTML.TagSoup             as TS
import           Text.Md.MdParserDef
import           Text.Md.ParseUtils
import           Text.Parsec                   (Parsec, ParsecT, Stream, (<?>),
                                                (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try)

data HtmlParseContext a = HtmlParseContext { parserText :: Parsec String ParseContext a }

pBlockElementDef :: Parsec String ParseContext Block
pBlockElementDef = pBlockElement $ HtmlParseContext (P.anyChar >>= \x -> return (Str [x]))

-- Parse html tags such as '<div><ul><li>list1</li><li>list2</li></ul></div>'
-- without considering whether the top tag is actually block element or not.
-- Assume that contents of the block element is escaped.
pBlockElement :: HtmlParseContext Inline -> Parsec String ParseContext Block
pBlockElement context = P.try $ do
  (tagStr, tagMaybe) <- pHtmlTag
  case tagMaybe of
    Just tag -> BlockHtml <$> liftM2 concatTags2 (return [Str tagStr]) (pBlockElementInside [tag] context)
    Nothing  -> return $ BlockHtml [Str tagStr]

pBlockElementInside []    context = return [Str ""]
pBlockElementInside stack context = P.try $ do
  text <- P.many (P.notFollowedBy (P.char '<') >> parserText context)
  (tagStr, tagMaybe) <- pHtmlTag
  case tagMaybe of
    Just tag -> case tag of
      TS.TagOpen name _ -> render text tagStr (tag:stack) context
      TS.TagClose name  -> render text tagStr (tail stack) context
      TS.TagComment str -> render text tagStr stack context
    Nothing  -> render text tagStr stack context

pInlineElement :: HtmlParseContext Inline -> Parsec String ParseContext Inline
pInlineElement context = P.try $ do
  (tagStr, tagMaybe) <- pHtmlTag
  case tagMaybe of
    Just tag -> InlineHtml <$> liftM2 concatTags2 (return [Str tagStr]) (pInlineElementInside [tag] context)
    Nothing  -> return $ InlineHtml [Str tagStr]

pInlineElementInside []    context = return [Str ""]
pInlineElementInside stack context = P.try $ do
  inlines <- P.many (P.notFollowedBy (P.char '<') >> parserText context)
  (tagStr, tagMaybe) <- pHtmlTag
  case tagMaybe of
    Just tag -> case tag of
      TS.TagOpen name _ -> renderInline inlines tagStr (tag:stack) context
      TS.TagClose name  -> renderInline inlines tagStr (tail stack) context
      TS.TagComment str -> renderInline inlines tagStr stack context
    Nothing  -> renderInline inlines tagStr stack context

pHtmlTag = do
  inside <- P.between (P.char '<') (P.char '>') (P.many1 (P.noneOf "<>"))
  let tags = TS.parseTags $ "<" ++ inside ++ ">"
  case length tags of
    1 -> return (TS.renderTags tags, Just (head tags))
    2 -> return (TS.renderTags tags, Nothing)

render text tagStr stack context = liftM3 concatTags3 (return text) (return [Str tagStr]) (pBlockElementInside stack context)

renderInline inlines tagStr stack context = liftM3 concatTags3 (return inlines) (return [Str tagStr]) (pInlineElementInside stack context)

concatTags2 a b = a ++ b

concatTags3 a b c = a ++ b ++ c

pHtmlEscape :: Parsec String ParseContext Inline
pHtmlEscape = do
  let pEscapedString        = P.choice (map (P.try . P.string . snd) escapePair)
      escapes               = map escape escapePair
      escape (raw, escaped) = P.string raw *> return (Str escaped)
  isEscaped <- P.optionMaybe $ P.try $ P.lookAhead pEscapedString
  case isEscaped of
    Just str -> Str <$> P.try pEscapedString
    Nothing  -> P.try (P.choice escapes <?> "html-parser")

escapePair = [("&", "&amp;"), ("<", "&lt;"), (">", "&gt;"), ("\"", "&quot;")]
