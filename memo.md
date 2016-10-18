### EBNF

- とりあえず改行コードは`\n`だけ考える。前処理で統一されるよう変換もできるので。
- 各行の末尾にnewlineがあることが前提になっている。

document  = paragraph, {(emptyLine, paragraph)}
paragraph = line, {line}
emptyLine = {(space | tab)}, newline
line      = anyChar, {anyChar}, newline
newline   = "\n"
space     = " "
tab       = '\t'
anyChar   = (any character but newline)

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
