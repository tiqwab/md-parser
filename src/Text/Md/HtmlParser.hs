{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.HtmlParser (
    HtmlParseContext(..)
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

-- | ParseContext used to handle html input.
data HtmlParseContext a = HtmlParseContext { parserText :: Parsec String ParseContext a -- parser used for text framed by html tags
                                           }
-- | Parse html tags such as '<div><ul><li>list1</li><li>list2</li></ul></div>'
-- without considering whether the top tag is actually block element or not.
pBlockElement :: HtmlParseContext Inline -> Parsec String ParseContext Block
pBlockElement context = BlockHtml <$> pHtmlElement context

-- | Parse html inline element.
-- Its work is almost same as `pBlockElement` but return `InlineHtml`.
pInlineElement :: HtmlParseContext Inline -> Parsec String ParseContext Inline
pInlineElement context = InlineHtml <$> pHtmlElement context

pHtmlElement context = P.try $ do
  (tagStr, tagMaybe) <- pHtmlTag
  case tagMaybe of
    Just tag -> liftM2 concatTags2 (return [Str tagStr]) (pHtmlElementInside [tag] context)
    Nothing  -> return [Str tagStr]

pHtmlElementInside []    context = return [Str ""]
pHtmlElementInside stack context = P.try $ do
  inlines <- P.many (P.notFollowedBy (P.char '<') >> parserText context)
  (tagStr, tagMaybe) <- pHtmlTag
  case tagMaybe of
    Just tag -> case tag of
      TS.TagOpen name _ -> render inlines tagStr (tag:stack) context
      TS.TagClose name  -> render inlines tagStr (tail stack) context
      TS.TagComment str -> render inlines tagStr stack context
    Nothing  -> render inlines tagStr stack context

pHtmlTag = do
  inside <- P.between (P.char '<') (P.char '>') (P.many1 (P.noneOf "<>"))
  let tags = TS.parseTags $ "<" ++ inside ++ ">"
  case length tags of
    1 -> return (TS.renderTags tags, Just (head tags)) -- when the tag has any inside elements. e.g. '<div>'
    2 -> return (TS.renderTags tags, Nothing)          -- when the tag has no inside elements. e.g. '<div />'

render inlines tagStr stack context = liftM3 concatTags3 (return inlines) (return [Str tagStr]) (pHtmlElementInside stack context)
concatTags2 a b = a ++ b
concatTags3 a b c = a ++ b ++ c

-- | Perform html escaping of '&', '<', '>', and '"'.
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
