{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Md.SimpleParser
where

import           Debug.Trace
import           System.IO
import           Text.Parsec                   (Parsec, ParsecT, (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try)

{- Parse document -}

pDocument = do x <- pParagraph
               y <- P.optionMaybe (pNewLine *> pDocument)
               case y of
                 Nothing -> return [doParse x]
                 Just z  -> return (doParse x : z)
  where doParse = parseBlock . unlines

pParagraph = P.many1 pLine

pLine = P.many1 (P.noneOf "\n") <* pNewLine

pNewLine = P.newline

-- P.space also accepts newline
pSpaces  = P.char ' ' <|> P.char '\t'

{- Parse block tags -}

-- Is it good way to parse again?
parseBlock input = case P.parse pBlock "" input of
                     Left  l -> "<p>" ++ trim (parseInline input) ++ "</p>"
                     Right r -> trim (parseInline r)
  where pBlock =  P.try pHead
              <|> P.try pBorder
              <|> P.try pLists

pHead = do hs <- countH <$> P.many1 (P.char '#')
           x <- P.many1 (P.noneOf "\n")
           return $ "<h" ++ hs ++ ">" ++ trim x ++ "</h" ++ hs ++">"
  where countH x = let c = length x
                   in show (min c 6)

pBorder = do bcount <- countB <$> P.many (P.char '-' <* P.many pSpaces)
             P.newline
             if bcount >= 3 then return "<hr/>" else fail "this is not border"
  where countB = length

pLists = (\x -> "<ul>" ++ x ++ "</ul>") <$> pList (-1)

pList n = do first  <- P.optionMaybe pListItem
             second <- P.optionMaybe (P.try (P.lookAhead pListItem))
             case first of
               Nothing -- come here when this block is not list
                 -> fail "pList"
               Just (firstItem, firstLevel)
                 -> case second of
                      Nothing
                        -> return $ "<li>" ++ firstItem ++ "</li>" ++ concat (replicate firstLevel "</ul></li>")
                      Just (secondItem, secondLevel)
                        | secondLevel == firstLevel ->
                          do x <- pList secondLevel
                             return $ "<li>" ++ firstItem ++ "</li>" ++ x
                        | secondLevel > firstLevel ->
                          do x <- pList secondLevel
                             return $ "<li>" ++ firstItem ++ "<ul>" ++ x
                        | otherwise ->
                          do x <- pList secondLevel
                             return $ "<li>" ++ firstItem ++ "</li>" ++ "</ul></li>" ++ x
  where
    pListItem = do actualN <- countN pSpaces
                   P.char '-'
                   P.space
                   item <- pLine
                   return (item, calcLevel actualN)
    countN p = do xs <- P.many p
                  return $ length xs
    calcLevel n = n `div` indent
    indent = 2

{- Parse inline tags -}
-- Is it good way to parse again?
-- How to parse remain input?
parseInline input = foldl (\acc p -> pInline p acc) input [applyEm, applyLink]
  where pInline p target = case P.parse p "" (target ++ "\n") of
                             Left  l -> input
                             Right r -> r
        applyEm = P.chainl1 (P.try pEmphasis <|> pLine) (pure (++))
        applyLink = P.chainl1 (P.try pLink <|> pLine) (pure (++))

pEmphasis = do plain <- P.manyTill P.anyChar (P.try (P.string "**"))
               em    <- P.manyTill P.anyChar (P.try (P.string "**"))
               return $ plain ++ "<strong>" ++ em ++ "</strong>"

pLink = do plain <- P.manyTill P.anyChar (P.try (P.string "["))
           title <- P.manyTill P.anyChar (P.try (P.string "]"))
           P.spaces *> P.char '('
           link  <- P.manyTill P.anyChar (P.try (P.string ")"))
           return $ plain ++ "<a href=\"" ++ link ++ "\">" ++ title ++ "</a>"

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
                  Right output -> foldl (\acc text -> acc ++ text ++ "\n") "" output

-- Parse Markdown in a file to HTML string.
parseMdFile :: FilePath -> IO String
parseMdFile file = withFile file ReadMode $ \handle -> do
  contents <- hGetContents handle
  putStrLn contents
  return (parseMd contents)
