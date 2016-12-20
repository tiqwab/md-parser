module Text.Md.HtmlTags (
    trim
  , hDiv
  , hHead
  , hList
  , hBorder
  , hCodeBlock
  , hQuote
  , hParagraph
  , hLineBreak
  , hStrong
  , hEmphasis
  , hCode
  , hLink
)
where

import Data.Char

-- | Trim string. This is not the efficient implementation.
trim :: String -> String
trim str = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace str

createTag True  name text = "<" ++ name ++ ">" ++ "\n" ++ text ++ "\n" ++ "</" ++ name ++ ">" ++ "\n" -- nsert newline between tag and inner text.
createTag False name text = "<" ++ name ++ ">" ++ text ++ "</" ++ name ++ ">"

hDiv True  text = createTag True "div" text
hDiv False text = createTag False "div" text

hHead level text = createTag False ("h" ++ show level) text

hList items = createTag False "ul" $ concatMap (createTag False "li") items

hBorder = "<hr />"

hCodeBlock text = createTag False "pre" $ createTag False "code" text

hQuote text = createTag False "blockquote" text

hParagraph text = createTag False "p" text

hLineBreak = "<br />"

hStrong text = createTag False "strong" text

hEmphasis text = createTag False "em" text

hCode text = createTag False "code" text

hLink text link (Just title) = "<a href=\"" ++ link ++ "\" title=\"" ++ title ++ "\">" ++ text ++ "</a>"
hLink text link Nothing      = "<a href=\"" ++ link ++ "\">" ++ text ++ "</a>"
