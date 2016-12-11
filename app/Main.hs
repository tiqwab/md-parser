module Main where

import           System.Environment
import qualified Text.Md.MdParser as MD

{-
> stack build
> cat samples/sample.md | stack exec mdparse
-}
main :: IO ()
main = do
  markdown <- getContents
  let html = MD.parseMarkdown markdown
  putStrLn html
