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
                 Nothing -> return [doParse x]
                 Just z  -> return ((doParse x) : z)
  where doParse = parseBlock . unlines

pParagraph = P.many1 pLine

pLine = P.many1 (P.noneOf "\n") <* pNewLine

pNewLine = P.newline

{- Parse block tags -}

-- Is it good way to parse again?
parseBlock input = case P.parse pBlock "" input of
                     Left  l -> "<p>" ++ trim input ++ "</p>"
                     Right r -> r
  where pBlock =  pHead
              <|> pBorder

pHead = do P.char '#'
           x <- P.many1 (P.noneOf "\n")
           return $ "<h1>" ++ trim x ++ "</h1>"

pBorder = do P.count 3 (P.char '-')
             return "<hr/>"

{- Utils -}

isBlankChar :: Char -> Bool
isBlankChar c = c `elem` ['\n', '\t', ' ']

-- This implementation is not efficient.
trim :: String -> String
trim s = let a = dropWhile isBlankChar s
         in  reverse . dropWhile isBlankChar $ reverse a

{- Parse functions -}

-- Process input before parsing.
preProcess :: String -> String
preProcess = unlines . map trimEmptyLine . lines
  where trimEmptyLine s
          | isEmptyLine s = ""
          | otherwise     = s
        isEmptyLine = foldr ((&&) . isBlankChar) True

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
