{-# LANGUAGE FlexibleContexts #-}

module Text.Md.ParseUtils
where

import Control.Monad
import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)

spaceChar :: Stream s m Char => ParsecT s st m Char
spaceChar = char ' ' <|> char '\t'

skipSpaces :: Stream s m Char => ParsecT s st m ()
skipSpaces = skipMany spaceChar

blankline :: Stream s m Char => ParsecT s st m Char
blankline = skipSpaces >> newline

blanklines :: Stream s m Char => ParsecT s st m String
blanklines = many1 blankline
