### EBNF

- とりあえず改行コードは`\n`だけ考える。前処理で統一されるよう変換もできるはずなので。
- 各行の末尾にnewlineがあることが前提になっている。
- いくつかのタグでMarkdown仕様上では表記法が複数存在するが、省略しているものがある。
  - border: '-'のみ
  - list: '-'のみ
  - emphasis: '\*'のみ
- ブロックタブの一部でうまく改行を処理できない(致命的)
  - list
  - code
- インラインレベルのパースで「残りのstring全てのパース」というのをどう実装するかが難しい。
  - 基本的にnewlineで見れると思っていたけど、段落中でもnewlineを使用しているので判断の難しい場面が多い。

document  = paragraph, {(emptyLine, paragraph)}
paragraph = line, {line}
emptyLine = {spaces}, newline
line      = anyChar, {anyChar}, newline
newline   = "\n"
space     = " "
tab       = '\t'
spaces    = (space | tab)
anyChar   = (any character but newline)

head      = "#", {"#"}, anyChar, newline
border    = ("-", {spaces}, "-", {spaces}, "-"), {("-" | spaces)}, newline
list      = listItem, {listItem}
listVal   = {spaces}, (anyChar but "-"), {anyChar}
listItem  = {spaces}, "-", spaces, (listVal, newline), {(listVal, newline)}
