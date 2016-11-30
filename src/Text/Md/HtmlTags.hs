module Text.Md.HtmlTags (
  hLink
)
where

hLink text link (Just title) = "<a href=\"" ++ link ++ "\" title=\"" ++ title ++ "\">" ++ text ++ "</a>"
hLink text link Nothing      = "<a href=\"" ++ link ++ "\">" ++ text ++ "</a>"
