{-# LANGUAGE FlexibleContexts #-}

import           Text.Parsec                   (Parsec, ParsecT, Stream, (<?>),
                                                (<|>))
import qualified Text.Parsec                   as P
import qualified Text.ParserCombinators.Parsec as P hiding (try, runParser)
import Control.Monad.Trans.State

parser char = P.many1 (P.char char)
parserS = StateT $ \x -> do
  y <- parser x
  return (y, x)

parserA :: Parsec String () String
parserA = do
  flip evalStateT 'a' $ do
    x <- parserS
    put 'b'
    y <- parserS
    return (x ++ y)

----------

data Node = Node Int String [Node]
  deriving (Show, Eq)

getNodesLevelAt 0     node@(Node l v cs) = (node, cs)
getNodesLevelAt level node@(Node l v cs) = getNodesLevelAt (level-1) (last cs)

insert 0     val node@(Node l v cs) = Node l v (cs ++ [ Node (l+1) val [] ])
insert level val node@(Node l v cs) = Node l v newCs
  where (formers, lastC) = splitAt (length cs - 1) cs
        newCs            = formers ++ fmap (insert (level-1) val) lastC

sample = [(1, "A"), (1, "B"), (2, "C"), (2, "D"), (1, "E"), (2, "F")]

toNode list = foldl summarize (Node 0 "root" []) list
  where summarize node (l, v) = insert (l-1) v node

toStr node = toLines node
  where toLines node@(Node 0 _ cs) = "<ul>" ++ concatMap toLines cs ++ "</ul>"
        toLines node@(Node l v []) = "<li>" ++ toLine node ++ "</li>"
        toLines node@(Node l v cs) = "<li>" ++ toLine node ++ "<ul>" ++ concatMap toLines cs ++ "</ul>" ++ "</li>"
        toLine  node@(Node l v cs) = v

----------

pBlockQuote :: Parsec String () String
pBlockQuote = concat <$> P.many1 pStr
  where pStr = P.many1 P.alphaNum <|> P.string " " <|> (P.newline >>= \x -> P.optional (P.char '>') >>= \_ -> return [x])

----------

-- P.many (parser h)
main = return ()
