{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.MdParser
where

import           Debug.Trace
import           System.IO
import           Text.Parsec                   (Parsec, ParsecT, Stream, (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try)

data Document = Document [Block]
  deriving (Show, Eq)

data Block = Paragraph [Inline]
  deriving (Show, Eq)

data Inline = Str String
  deriving (Show, Eq)

class ReadMd a where
  parser :: (Stream s m Char) => ParsecT s st m a

instance ReadMd Document where
  parser = do blocks <- P.manyTill parser P.eof
              return $ Document blocks

instance ReadMd Block where
  parser = do inlines <- P.many1 parser
              return $ Paragraph inlines

instance ReadMd Inline where
  parser = do str <- P.many1 P.letter
              return $ Str str

class WriteMd a where
  writeMd :: a -> String

instance WriteMd Document where
  writeMd (Document blocks) = "<div>" ++ concatMap writeMd blocks ++ "</div>"

instance WriteMd Block where
  writeMd (Paragraph inlines) = "<p>" ++ concatMap writeMd inlines ++ "</p>"

instance WriteMd Inline where
  writeMd (Str str) = str

readMarkdown :: String -> Document
readMarkdown input = case P.parse parser "" input of
                       Left  e -> error (show e) -- FIX ME
                       Right s -> s

writeMarkdown :: Document -> String
writeMarkdown doc = writeMd doc

parseMarkdown :: String -> String
parseMarkdown = writeMarkdown . readMarkdown
