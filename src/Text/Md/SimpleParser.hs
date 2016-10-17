{-# LANGUAGE OverloadedStrings #-}

module Text.Md.SimpleParser
where

import           System.IO
import           Text.Parsec                   ((<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try)

pDocument = do x <- pParagraph
               y <- P.optionMaybe (pNewLine *> pDocument)
               case y of
                 Nothing -> return [x]
                 Just z  -> return (x : z)

pParagraph = P.many1 pLine

pLine = P.many1 (P.noneOf "\n") <* pNewLine

pNewLine = P.newline

-- Process input before parsing.
preProcess :: String -> String
preProcess = unlines . map trimEmptyLine . lines
  where trimEmptyLine s
          | isEmptyLine s = ""
          | otherwise     = s
        isEmptyLine = foldr (\x -> (&&) (x `elem` ['\n', '\t', ' '])) True

-- Parse Markdown to HTML string.
parseMd :: String -> String
parseMd input = let parsed = P.parse pDocument "" (preProcess input)
                in case parsed of
                  Left  e      -> show e
                  Right output -> show output

-- Parse Markdown in a file to HTML string.
parseMdFile :: FilePath -> IO String
parseMdFile file = withFile file ReadMode $ \handle -> do
  contents <- hGetContents handle
  return (parseMd contents)
