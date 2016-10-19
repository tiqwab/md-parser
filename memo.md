### EBNF

- とりあえず改行コードは`\n`だけ考える。前処理で統一されるよう変換もできるので。
- 各行の末尾にnewlineがあることが前提になっている。
- いくつかのタグでMarkdown仕様上では表記法が複数存在するが、省略しているものがある。
  - border: '-'のみ
  - list: '-'のみ
- ブロックタブの一部でうまく改行を処理できない(致命的)
  - list
  - code

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

---

Hi,
I am Taro.

- Alice
- Bob
- Chris

Nice to meet you.

---

<p>Hi,
I am Taro.</p>

<ul>
<li>Alice</li>
<li>Bob</li>
<li>Chris</li>
</ul>

<p>Nice to meet you.</p>
