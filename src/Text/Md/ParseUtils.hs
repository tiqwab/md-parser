{-# LANGUAGE FlexibleContexts #-}

module Text.Md.ParseUtils
where

import Data.Maybe
import Control.Monad
import Text.Parsec (Stream, ParsecT, (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (runParser, try)
import Text.Md.MdParserDef

spaceChar :: Stream s m Char => ParsecT s ParseContext m Char
spaceChar = P.char ' ' <|> P.char '\t'

skipSpaces :: Stream s m Char => ParsecT s ParseContext m ()
skipSpaces = P.skipMany spaceChar

newlineQuote :: Stream s m Char => ParsecT s ParseContext m Char
newlineQuote = do
  c <- lineStart <$> P.getState
  nl <- P.newline
  isQuoted <- isJust <$> P.optionMaybe (P.char c)
  P.modifyState (\context -> context { isLastNewLineQuoted = isQuoted })
  when isQuoted (P.optional spaceChar)
  return nl

blankline :: Stream s m Char => ParsecT s ParseContext m Char
blankline = skipSpaces >> newlineQuote

blanklines :: Stream s m Char => ParsecT s ParseContext m String
blanklines = P.many1 blankline
