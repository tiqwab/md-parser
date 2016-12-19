{-# LANGUAGE FlexibleContexts #-}

module Text.Md.ParseUtils
where

import Data.Maybe
import Debug.Trace
import Control.Monad
import Text.Parsec (Stream, ParsecT, (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (runParser, try)
import Text.Md.MdParserDef

-- | Parse one character of ' ' or '\t'.
spaceChar :: Stream s m Char => ParsecT s ParseContext m Char
spaceChar = P.char ' ' <|> P.char '\t'

-- | Skip ' ' or '\t' more than zero times.
skipSpaces :: Stream s m Char => ParsecT s ParseContext m ()
skipSpaces = P.skipMany spaceChar

-- | Parse a newline.
-- This method also handles quote marks at the beginning of lines and update the parser state.
newlineQuote :: Stream s m Char => ParsecT s ParseContext m Char
newlineQuote = do
  c <- lineStart <$> P.getState
  level <- quoteLevel <$> P.getState
  nl <- P.newline
  -- `P.optionMaybe (P.try (P.count level xxx))` parses no input if xxx does not occurs `level` times.
  isQuoted <- isJust <$> P.optionMaybe (P.try (P.count level (P.char c <* P.optional spaceChar)))
  -- consume quotes when `quote` does not occur `level` times
  unless isQuoted (void (P.many (P.char c <* P.optional spaceChar)))
  P.modifyState (\context -> context { isLastNewLineQuoted = isQuoted })
  return nl

-- | Parse a blank line.
blankline :: Stream s m Char => ParsecT s ParseContext m Char
blankline = skipSpaces >> newlineQuote

-- | Parse blank lines.
blanklines :: Stream s m Char => ParsecT s ParseContext m String
blanklines = P.many1 blankline
