module Text.Md.HtmlTags (
  trim,
  hDiv,
  hHead,
  hList,
  hBorder,
  hCodeBlock,
  hQuote,
  hParagraph,
  hLineBreak,
  hStrong,
  hEmphasis,
  hCode,
  hLink
)
where

import Data.Char

-- | Trim string. This is not the efficient implementation.
trim :: String -> String
trim str = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace str

hDiv text = "<div>" ++ text ++ "</div>"

hHead level text = "<h" ++ show level ++ ">" ++ text ++ "</h" ++ show level ++ ">"

hList items = start ++ concatMap encloseItem items ++ end
  where start            = "<ul>"
        end              = "</ul>"
        encloseItem item = "<li>" ++ item ++ "</li>"

hBorder = "<hr />"

hCodeBlock text = "<pre><code>" ++ text ++ "</code></pre>"

hQuote text = "<blockquote>" ++ text ++ "</blockquote>"

hParagraph text = "<p>" ++ text ++ "</p>"

hLineBreak = "<br />"

hStrong text = "<strong>" ++ text ++ "</strong>"

hEmphasis text = "<em>" ++ text ++ "</em>"

hCode text = "<code>" ++ text ++ "</code>"

hLink text link (Just title) = "<a href=\"" ++ link ++ "\" title=\"" ++ title ++ "\">" ++ text ++ "</a>"
hLink text link Nothing      = "<a href=\"" ++ link ++ "\">" ++ text ++ "</a>"
