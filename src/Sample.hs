{-# LANGUAGE FlexibleContexts #-}

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

-- now cannot handle space at the end of lines
readLines :: Stream s m Char => ParsecT s st m [String]
readLines = many (notFollowedBy blanklines >> many1 letter <* blanklines)

{-
main = do input <- sequence [getLine, getLine, getLine, getLine]
          parseTest readLines $ unlines input
-}
---

-- `eof`はinputの終わりとして使うことができる。
-- `manyTill p end`のパースが成功するのは0以上のpのパースが成功した直後にendのパースが成功したとき。
{-
manyTill :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill p end      = scan
                    where
                      scan  = do{ end; return [] }
                            <|>
                              do{ x <- p; xs <- scan; return (x:xs) }
-}
-- parseTest readCsv "col,coll,colll"
-- -> ["col","coll","colll"]
readCsv :: Stream s m Char => ParsecT s st m [String]
readCsv = manyTill (try (  many1 letter <* char ',')
                       <|> many1 letter)
                        eof

---

-- `notFollowedBy p`は単体で使われるとその名前と挙動が一致せず少し理解しづらい。
-- pのパースが失敗したとき、`notFollowedBy`としてのパースは成功する。また、その際にinputは消費されない。
{-
notFollowedBy :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m ()
notFollowedBy p     = try (do{ c <- try p; unexpected (show c) }
                           <|> return ()
                          )
-}
-- parseTest parseSample "abc123d456efg\n"
-- -> ["abc","123","d","456","efg"]
parseSample :: Stream s m Char => ParsecT s st m [String]
parseSample = many (notFollowedBy closingSample >> contentSample)
  where closingSample = blanklines
        contentSample = many1 letter <|> many1 digit

---

ender' :: Stream s m Char => Char -> Int -> ParsecT s st m ()
ender' c n = try $ do
  count n (char c)
  guard (c == '*')
    <|> notFollowedBy alphaNum

main = return ()
