module Text.Md.HtmlTags (
  trim,
  hLink
)
where

import Data.Char

-- | Trim string. This is not implemented in the efficient way.
trim :: String -> String
trim str = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace str

hLink text link (Just title) = "<a href=\"" ++ link ++ "\" title=\"" ++ title ++ "\">" ++ text ++ "</a>"
hLink text link Nothing      = "<a href=\"" ++ link ++ "\">" ++ text ++ "</a>"
