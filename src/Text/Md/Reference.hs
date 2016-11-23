{-# LANGUAGE FlexibleContexts #-}

import           Text.Parsec                   (Parsec, ParsecT, Stream, (<?>),
                                                (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try, runParser)
import qualified Data.Map as M
import System.Environment

type Key = String
type LinkRef = Maybe String

data ParserState = ParserState { referenceTable :: M.Map Key LinkRef }

parser = do
  let pCharAsStr = (:[]) <$> P.anyChar
  result <- P.manyTill ( P.try parseLinkRef
                     <|> pCharAsStr) P.eof
  state <- P.getState
  return (concat result, referenceTable state)

parseLinkRef = do
  key <- pEnclosed "[" "]"
  ref <- pEnclosed "[" "]"
  P.updateState (addRef key ref)
  return ""

pEnclosed begin end = P.between pBegin pEnd (P.many1 pNones)
  where pBegin = P.string begin
        pEnd   = P.string end
        pNones = P.noneOf (begin ++ end)

addRef key ref state = state { referenceTable = newTable }
  where newTable = M.insert key (Just ref) $ referenceTable state

main = do
  line <- head <$> getArgs
  case P.runParser parser (ParserState M.empty) "" line of
    Left e  -> print e
    Right p -> print p
