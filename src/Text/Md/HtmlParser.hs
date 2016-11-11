{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.HtmlParser (
  pBlockElement,
)
where

import           Control.Monad
import           Debug.Trace
import           System.IO
import qualified Text.HTML.TagSoup             as TS
import           Text.Md.ParseUtils
import           Text.Parsec                   (Parsec, ParsecT, Stream, (<?>),
                                                (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try)

-- parse html tags such as '<div><ul><li>list1</li><li>list2</li></ul></div>'
-- without considering whether the top tag is block element or not
pBlockElement :: Parsec String () String
pBlockElement = P.try $ do
  (tagStr, tagMaybe) <- pHtmlTag
  case tagMaybe of
    Just tag -> liftM2 concatTags2 (return tagStr) (pBlockElementInside [tag])
    Nothing  -> return tagStr

pBlockElementInside []    = return ""
pBlockElementInside stack = P.try $ do
  text <- P.many (P.notFollowedBy (P.char '<') >> P.anyChar)
  (tagStr, tagMaybe) <- pHtmlTag
  case tagMaybe of
    Just tag -> case tag of
      TS.TagOpen name _ -> render text tagStr (tag:stack)
      TS.TagClose name  -> render text tagStr (tail stack)
      TS.TagComment str -> render text tagStr stack
    Nothing  -> render text tagStr stack

pHtmlTag = do
  inside <- P.between (P.char '<') (P.char '>') (P.many1 (P.noneOf "<>"))
  let tags = TS.parseTags $ "<" ++ inside ++ ">"
  case length tags of
    1 -> return (TS.renderTags tags, Just (head tags))
    2 -> return (TS.renderTags tags, Nothing)

render text tagStr stack = liftM3 concatTags3 (return text) (return tagStr) (pBlockElementInside stack)

concatTags2 a b = a ++ b

concatTags3 a b c = a ++ b ++ c
