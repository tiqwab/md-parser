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
- 同じところを何回もパースしているのが無駄に感じる。

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

### pandoc読みこなし

ドキュメント、ソースの参考にしているversionは`pandoc-1.17.2`。

- `Reader`と`Writer`というデータ型があり、それぞれinputとoutputのフォーマットに対応する。
- 各フォーマット, `Reader`, `Writer`単位でパッケージがわけられているのでわかりやすい。
- パースには`parsec`パッケージが使用されている。
- Markdownを読みHTMLを吐き出したい場合、(エラー処理を無視すれば)コード上は以下のようになる。
  - `read〜`関数は`ReaderOptions -> String -> Either PandocError Pandoc`と定義。
  - `write〜`関数は`WriterOptions -> Pandoc -> String`と定義。
    - なのでパース時のデータの捉え方は以下のようになる。
      - `Reader`を`read〜`関数から作り、パース結果を`Pandoc`データ型として持つ。
      - `Writer`を`write〜`関数から作り、`Pandoc`からフォーマット結果を`String`として出力する。
  - `def`は`Text.Pandoc.Options`で`def :: Default a => a`と定義。
    - `ReaderOptions`や`WriterOptions`のデフォルト値を表す。

```haskell
{-
公式のdocumentを元にしたコード。もともとは関数合成順が`writeHtml`と`handleError`で逆だったけれど、それでは型的に動かないはずでは。
-}
-- Convert markdown to HTML
readMarkdownToHtml :: String -> String
readMarkdownToHtml = writeHtmlString def . handleError . readMarkdown def
```

- `Pandoc`は特定のフォーマットに依存しないメタ的な文書構造を表すデータ型。
  - `data Pandoc = Pando Meta [Block]`で定義。
  - `Meta`は文書のメタデータを表すデータ型(タイトル、筆者etc)。
  - `Block`は段落というかHTMLのブロック要素に当たるものというか。下の定義を見た方が早いか。
  - `Inline`は文というかHTMLのインライン要素に当たるものというか。下の定義を見た方が早いか。

```haskell
-- | Block element.
data Block
    = Plain [Inline]           -- Plain text, not a paragraph
    | Para [Inline]            -- Paragraph
    | CodeBlock Attr String    -- Code block (literal) with attributes
    | RawBlock Format String   -- Raw block
    | BlockQuote [Block]       -- Block quote (list of blocks)
    | OrderedList ListAttributes [[Block]] -- Ordered list (attributes
                               -- and a list of items, each a list of blocks)
    | BulletList [[Block]]     -- Bullet list (list of items, each
                               -- a list of blocks)
    | DefinitionList [([Inline],[[Block]])]  -- Definition list
                               -- Each list item is a pair consisting of a
                               -- term (a list of inlines) and one or more
                               -- definitions (each a list of blocks)
    | Header Int Attr [Inline] -- Header - level (integer) and text (inlines)
    | HorizontalRule           -- Horizontal rule
    | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- Table,
                               -- with caption, column alignments (required),
                               -- relative column widths (0 = default),
                               -- column headers (each a list of blocks), and
                               -- rows (each a list of lists of blocks)
    | Div Attr [Block]         -- Generic block container with attributes
    | Null                     -- Nothing
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
```

```haskell
-- | Inline elements.
data Inline
    = Str String                 -- Text (string)
    | Emph [Inline]              -- Emphasized text (list of inlines)
    | Strong [Inline]            -- Strongly emphasized text (list of inlines)
    | Strikeout [Inline]         -- Strikeout text (list of inlines)
    | Superscript [Inline]       -- Superscripted text (list of inlines)
    | Subscript [Inline]         -- Subscripted text (list of inlines)
    | SmallCaps [Inline]         -- Small caps text (list of inlines)
    | Quoted QuoteType [Inline]  -- Quoted text (list of inlines)
    | Cite [Citation]  [Inline]  -- Citation (list of inlines)
    | Code Attr String           -- Inline code (literal)
    | Space                      -- Inter-word space
    | SoftBreak                  -- Soft line break
    | LineBreak                  -- Hard line break
    | Math MathType String       -- TeX math (literal)
    | RawInline Format String    -- Raw inline
    | Link Attr [Inline] Target  -- Hyperlink: alt text (list of inlines), target
    | Image Attr [Inline] Target -- Image:  alt text (list of inlines), target
    | Note [Block]               -- Footnote or endnote
    | Span Attr [Inline]         -- Generic inline container with attributes
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
```

- 入力データ内のnewlineは`\n`でなくてはならない。

#### Text.Pandoc.Readers.Markdown

- Markdownのパースを担当しているパッケージ。

以下のコード断片は他に言及が無ければText.Pandoc.Readers.Markdownパッケージからのもの。`memo:`と先頭につけているコメント文はオリジナルのコード上のものではなく自分で付加したもの。

Markdownのパーサを表すデータ型として`MarkdownParser`が定義されている。

```haskell
{- memo:
`MarkdownParser a` is an alias of parser consuming `String` and produce `a`(* -> *).
`ParserState` is defined in `Text.Pandoc.Parsing`
 -}
type MarkdownParser = Parser [Char] ParserState
```

```haskell
{- memo: From Text.Pandoc.Parsing -}
type Parser t s = Parsec t s
type ParserT = ParsecT
```

Markdown -> `Pandoc`データへの変換には`parseMarkdown`関数が使用される。`Pandoc`を構成する複数の`Block`データは`parseBlocks`関数で生成される。

```haskell
{- memo: alias 'B' means Text.Pandoc.Builder package -}
parseMarkdown :: MarkdownParser Pandoc
parseMarkdown = do
  -- markdown allows raw HTML
  updateState $ \state -> state { stateOptions =
                let oldOpts = stateOptions state in
                    oldOpts{ readerParseRaw = True } }
  optional titleBlock
  blocks <- parseBlocks
  st <- getState
  let meta = runF (stateMeta' st) st
  let Pandoc _ bs = B.doc $ runF blocks st
  eastAsianLineBreaks <- option False $
                    True <$ guardEnabled Ext_east_asian_line_breaks
  return $ (if eastAsianLineBreaks
               then bottomUp softBreakFilter
               else id) $ Pandoc meta bs

parseBlocks :: MarkdownParser (F Blocks)
parseBlocks = mconcat <$> manyTill block eof
```

`F`ってなんじゃいという感じなので以下の定義を確認。

```haskell
{- memo: From Text.Pandoc.Parsing -}
newtype F a = F { unF :: Reader ParserState a } deriving (Monad, Applicative, Functor)

runF :: F a -> ParserState -> a
runF = runReader . unF

instance Monoid a => Monoid (F a) where
  mempty = return mempty
  mappend = liftM2 mappend
  mconcat = liftM mconcat . sequence
```

```haskell
{- memo:
`tr` is a flag to output debug log if necessary.
`res` stores the result of parse blocks.
 -}
block :: MarkdownParser (F Blocks)
block = do
  tr <- getOption readerTrace
  pos <- getPosition
  res <- choice [ mempty <$ blanklines
               , codeBlockFenced
               , yamlMetaBlock
               -- note: bulletList needs to be before header because of
               -- the possibility of empty list items: -
               , bulletList
               , header
               , lhsCodeBlock
               , divHtml
               , htmlBlock
               , table
               , codeBlockIndented
               , guardEnabled Ext_latex_macros *> (macro >>= return . return)
               , rawTeXBlock
               , lineBlock
               , blockQuote
               , hrule
               , orderedList
               , definitionList
               , noteBlock
               , referenceKey
               , abbrevKey
               , para
               , plain
               ] <?> "block"
  when tr $ do
    st <- getState
    trace (printf "line %d: %s" (sourceLine pos)
           (take 60 $ show $ B.toList $ runF res st)) (return ())
  return res
```

Block要素のパース例として`header`(見出し。e.g. `<h3>`。Markdown上では`### Header3`のように表される)を見てみる。

```haskell
header :: MarkdownParser (F Blocks)
header = setextHeader <|> atxHeader <?> "header"

{- memo:
`withRaw` defined in Text.Pandoc.Parsing package receives `ParsecT` and return the result of parsing and raw input.
-}
atxHeader :: MarkdownParser (F Blocks)
atxHeader = try $ do
  level <- atxChar >>= many1 . char >>= return . length
  notFollowedBy $ guardEnabled Ext_fancy_lists >>
                  (char '.' <|> char ')') -- this would be a list
  skipSpaces
  (text, raw) <- withRaw $
          trimInlinesF . mconcat <$> many (notFollowedBy atxClosing >> inline)
  attr <- atxClosing
  attr' <- registerHeader attr (runF text defaultParserState)
  guardDisabled Ext_implicit_header_references
    <|> registerImplicitHeader raw attr'
  return $ B.headerWith attr' level <$> text

atxClosing :: MarkdownParser Attr
atxClosing = try $ do
  attr' <- option nullAttr
             (guardEnabled Ext_mmd_header_identifiers >> mmdHeaderIdentifier)
  skipMany . char =<< atxChar
  skipSpaces
  attr <- option attr'
             (guardEnabled Ext_header_attributes >> attributes)
  blanklines
  return attr
```

- 重要なのは`atxHeader`関数の`level`を取得している箇所と`(test, raw)`を取得している箇所。`level`には見出しのレベル、`text`には見出しの内容が入る。
- `axtHeader`関数内の`many (notFollowedBy atxClosing >> inline)`というのが自分の一番理解したいところ。ここで`Block`データを構成する`[Inlines]`のパースが行われている。`notFollowedBy p`でpのパースが失敗したとき、`notFollowedBy`としてのパースは成功する(このときinputは消費されない)ので、ここでは`axtClosing`のパースが失敗したとき、`inline`のパースが実行される。`atxClosing`のパースが成功するとき、これは恐らく`blanklines`のパースが成功するときであり、改行が見出しの末尾になるのはMarkdownから考えても正しい。
- `inline`のパースは`MarkdownParser (F Inlines)`を返す。`F a`の`mconcat`は`Text.Pandoc.Parsing`、`Inlines`のMonoid実装は`Text.Pandoc.Builder`で定義されており、`mconcat`でインライン要素同士をまとめるような動きになる(`Inlines`のMonoid実装はもう少しちゃんと見たい)。

```haskell
inline :: MarkdownParser (F Inlines)
inline = choice [ whitespace
                , bareURL
                , str
                , endline
                , code
                , strongOrEmph
                , note
                , cite
                , link
                , image
                , math
                , strikeout
                , subscript
                , superscript
                , inlineNote  -- after superscript because of ^[link](/foo)^
                , autoLink
                , spanHtml
                , rawHtmlInline
                , escapedChar
                , rawLaTeXInline'
                , exampleRef
                , smart
                , return . B.singleton <$> charRef
                , emoji
                , symbol
                , ltSign
                ] <?> "inline"
```

とりあえず一番イメージがしやすそうな`strongOrEmph`の実装を見てみる。Markdown上で`*this is emphasized*`、`**this is strong**`のようなinputのパースとなる。

```haskell
strongOrEmph :: MarkdownParser (F Inlines)
strongOrEmph =  enclosure '*' <|> enclosure '_'

{- memo: `<>`` is an infix synonym for `mappend`. -}
-- Parses material enclosed in *s, **s, _s, or __s.
-- Designed to avoid backtracking.
enclosure :: Char
          -> MarkdownParser (F Inlines)
enclosure c = do
  -- we can't start an enclosure with _ if after a string and
  -- the intraword_underscores extension is enabled:
  guardDisabled Ext_intraword_underscores
    <|> guard (c == '*')
    <|> (guard =<< notAfterString)
  cs <- many1 (char c)
  (return (B.str cs) <>) <$> whitespace
    <|> do
        case length cs of
             3  -> three c
             2  -> two   c mempty
             1  -> one   c mempty
             _  -> return (return $ B.str cs)
```

例のごとく細かいところには飛び込まず概要だけさらってみると、

- `cs <- many1 (char c)`で始まりの'\*'1つ以上をパースしている。
- `(return (B.str cs) <>) <$> whitespace`は残りがspace的なものだけならば'\*'をただの文字列として返して終了するということ。`whitespace`の実装は以下の通り。

```haskell
whitespace :: MarkdownParser (F Inlines)
whitespace = spaceChar >> return <$> (lb <|> regsp) <?> "whitespace"
  where lb = spaceChar >> skipMany spaceChar >> option B.space (endline >> return B.linebreak)
        regsp = skipMany spaceChar >> return B.space
```

- そうではない場合、パースに成功した'\*'の個数に応じた関数が呼ばれる。

```haskell
ender :: Char -> Int -> MarkdownParser ()
ender c n = try $ do
  count n (char c)
  guard (c == '*')
    <|> guardDisabled Ext_intraword_underscores
    <|> notFollowedBy alphaNum

-- Parse inlines til you hit one c or a sequence of two cs.
-- If one c, emit emph and then parse two.
-- If two cs, emit strong and then parse one.
-- Otherwise, emit ccc then the results.
three :: Char -> MarkdownParser (F Inlines)
three c = do
  contents <- mconcat <$> many (notFollowedBy (ender c 1) >> inline)
  (ender c 3 >> return ((B.strong . B.emph) <$> contents))
    <|> (ender c 2 >> one c (B.strong <$> contents))
    <|> (ender c 1 >> two c (B.emph <$> contents))
    <|> return (return (B.str [c,c,c]) <> contents)

-- Parse inlines til you hit two c's, and emit strong.
-- If you never do hit two cs, emit ** plus inlines parsed.
two :: Char -> F Inlines -> MarkdownParser (F Inlines)
two c prefix' = do
  contents <- mconcat <$> many (try $ notFollowedBy (ender c 2) >> inline)
  (ender c 2 >> return (B.strong <$> (prefix' <> contents)))
    <|> return (return (B.str [c,c]) <> (prefix' <> contents))

-- Parse inlines til you hit a c, and emit emph.
-- If you never hit a c, emit * plus inlines parsed.
one :: Char -> F Inlines -> MarkdownParser (F Inlines)
one c prefix' = do
  contents <- mconcat <$> many (  (notFollowedBy (ender c 1) >> inline)
                           <|> try (string [c,c] >>
                                    notFollowedBy (ender c 1) >>
                                    two c mempty) )
  (ender c 1 >> return (B.emph <$> (prefix' <> contents)))
    <|> return (return (B.str [c]) <> (prefix' <> contents))
```

- `one`, `two`, `three`と関数が分かれているのは`strong`と`em`の入れ子を処理するため。`strong`と`em`で入れ子を作るというのはあまり使用したことがないけれど、パース例としては以下のようになる。
  - `**foo*bar***`は`<p><strong>foo<em>bar</em></strong></p>`
  - `***foobar***`は`<p><strong><em>foobar</em></strong></p>`
- `strong`のみでの入れ子、`em`のみでの入れ子は逆にmozillaの資料にも明記されている([参考][1])。その場合Markdown的にどうすればいいのかは不明。少なくともpandocでは`\*`と`_`を使い分けて2階層までは書くことができる。
  - `__foo**bar**__`は`<p><strong>foo<strong>bar</strong></strong></p>`
  - ただ`**foo__bar__**`は`<p>**foo__bar__**</p>`となってしまう
    - [issue][2]に少し似たようなものがある。
- インライン要素なので末尾のパースに改行は使わない。

---

#### 補足

- `bottomUp`
  - `Text.Pandoc.Generic`で定義。
  - `Data.Generics`の`everywhere`, `mkT`を使用。
  - 一度`Pandoc`を作った上で、`Block`や`Inline`に何らかの変換を施したいときに使われているよう。
- `notFollowedBy :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m ()`
  - `Text.Parsec`で定義。
  - ヘッダのパース中に`many (notFollowedBy atxClosing >> inline)`として使用されているが、その意味を最初捉え間違えていた。
  - `atxClosing`が後ろに続かない文字列をまとめて`inline`に渡すのではなく、`atxClosing`が続かないことを確認したら`inline`にあとは任せる挙動になる。その際、`notFollowedBy`によってinputが消費されることはない。
- ヘッダのパース時に使用される`atxClosing`とInlinesの`endline`の区別はどうなっている？
  - どちらも改行を対象にする。
  - ただヘッダのパースは`many (notFollowedBy atxClosing >> inline)`という定義なので、ヘッダのパース中に見つかる改行は全て`atxClosing`の処理対象となる。

#### 方針

- パーサを定義する関数は`manyTill (some parser) eof`
  - inputの終わりは`eof`で捉える。
- Document, Block, Inlineのような階層構造を意識する。
  - Documentは文書全体を表し、1つ以上のBlockから構成される。
  - Blockはパラグラフ(のようなもの)を表し、1つ以上のInlineから構成される。
  - Inlineは修飾付きの文章。
- 各Blockは異なるprefixを持つはずなので、それをパース開始の目印とする。postfixは必ず持つとは限らないが、少なくとも最後は改行とみなしてよいはず。


[1]: https://developer.mozilla.org/ja/docs/Web/HTML/Element/strong
[2]: https://github.com/jgm/pandoc/issues/2613
