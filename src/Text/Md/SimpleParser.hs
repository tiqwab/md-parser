{-# LANGUAGE OverloadedStrings #-}

module Text.Md.SimpleParser
where

import           System.IO
import           Text.Parsec                   ((<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try)

{-
pBody = codeBody <$> (P.many P.anyChar <* P.notFollowedBy (P.char '\n')) `P.sepBy` pNewLines
  where codeBody = foldl1 (++) . map (\s -> "<p>" ++ s ++ "</p>")
        pNewLines = P.newline *> P.many1 P.newline
-}

{-
pBody = sequence [pParagraph, pParagraph]

pParagraph = codeParagraph <$> (  P.try (P.manyTill P.anyChar pNewLines)
                              <|> P.many P.anyChar  )
  where codeParagraph s = "<p>" ++ s ++ "</p>"
        pNewLines  = P.try (P.newline *> P.many1 P.newline)
-}

{-
pBody = do parts <- P.many P.letter `P.sepBy` P.count 2 P.newline
           sequence . fmap pId $ parts
  where pId s = return ("<p>" ++ s ++ "</p>")
-}

pDocument = do x <- pParagraph
               y <- P.optionMaybe (pEmptyLine *> pDocument)
               case y of
                 Nothing -> return [x]
                 Just z  -> return (x : z)

pParagraph = P.many1 pLine

pEmptyLine = P.skipMany (P.char ' ' <|> P.char '\t') *> P.newline

pLine = P.many1 (P.noneOf "\n") <* pNewLine

pNewLine = P.newline

parseMd :: String -> String
parseMd input = let parsed = P.parse pDocument "" input
                in case parsed of
                  Left  e      -> show e
                  Right output -> show output

parseMdFile :: FilePath -> IO String
parseMdFile file = withFile file ReadMode $ \handle -> do
  contents <- hGetContents handle
  return (parseMd contents)
