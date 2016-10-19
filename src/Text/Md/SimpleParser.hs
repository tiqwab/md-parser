{-# LANGUAGE OverloadedStrings #-}

module Text.Md.SimpleParser
where

import           System.IO
import           Text.Parsec                   (Parsec, ParsecT, (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try)

{- Parse document -}

pDocument = do x <- pParagraph
               y <- P.optionMaybe (pNewLine *> pDocument)
               case y of
                 Nothing -> return [doParse x]
                 Just z  -> return ((doParse x) : z)
  where doParse = parseBlock . unlines

pParagraph = P.many1 pLine

pLine = P.many1 (P.noneOf "\n") <* pNewLine

pNewLine = P.newline

-- P.space also accepts newline
pSpaces  = P.char ' ' <|> P.char '\t'

{- Parse block tags -}

-- Is it good way to parse again?
parseBlock input = case P.parse pBlock "" input of
                     Left  l -> "<p>" ++ trim input ++ "</p>"
                     Right r -> r
  where pBlock =  P.try pHead
              <|> P.try pBorder
              <|> P.try pList

pHead = do hs <- countH <$> P.many1 (P.char '#')
           x <- P.many1 (P.noneOf "\n")
           return $ "<h" ++ hs ++ ">" ++ trim x ++ "</h" ++ hs ++">"
  where countH x = let c = length x
                   in show (min c 6)

pBorder = do bcount <- countB <$> P.many (P.char '-' <* P.many pSpaces)
             P.newline
             if bcount >= 3 then return "<hr/>" else fail "this is not border"
  where countB = length

pList = do xs <- P.many1 pListItem
           let items =  foldr1 (++) . map (\x -> "<li>" ++ x ++ "</li>") $ xs
           return $ "<ul>" ++ items ++ "</ul>"
  where pListItem = do P.char '-'
                       P.space
                       pLine

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
