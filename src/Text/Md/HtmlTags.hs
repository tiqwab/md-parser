module Text.Md.HtmlTags (
  trim,
  hList,
  hLink
)
where

import Data.Char

-- | Trim string. This is not implemented in the efficient way.
trim :: String -> String
trim str = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace str

hList items = start ++ concatMap encloseItem items ++ end
  where start            = "<ul>"
        end              = "</ul>"
        encloseItem item = "<li>" ++ item ++ "</li>"

hLink text link (Just title) = "<a href=\"" ++ link ++ "\" title=\"" ++ title ++ "\">" ++ text ++ "</a>"
hLink text link Nothing      = "<a href=\"" ++ link ++ "\">" ++ text ++ "</a>"
