module Text.Md.MdParserSpec (
  main,
  spec,
) where

import           Test.Hspec
import           Text.Md.MdParser

-- Not necessary for automatic test discovery
main :: IO ()
main = hspec spec

-- Must export `spec` as type of `Spec`
spec :: Spec
spec = do
  describe "parseMd" $ do
    it "parses to paragraph" $
      parseMd "foobar\n\n"
      `shouldBe`
      "<div><p>foobar</p></div>"

    it "parses to header" $ do
      parseMd "# header1\n\npara1\n\n##   header2\n\n### header3\n\n"
      `shouldBe`
      "<div><h1>header1</h1><p>para1</p><h2>header2</h2><h3>header3</h3></div>"

    it "parses html block elements to blocks without framing by 'p' tag" $ do
      parseMd "line1\n\n<div id=\"header\">**line2**</div>\n\nline3\n\n" `shouldBe` "<div><p>line1</p><div id=\"header\">**line2**</div><p>line3</p></div>"
      parseMd "line1\n\n<div><ul><li>list1</li><li>list2</li>\n\n</ul></div>\n\nline3\n\n" `shouldBe` "<div><p>line1</p><div><ul><li>list1</li><li>list2</li>\n\n</ul></div><p>line3</p></div>"
      -- attributes and text framed by tags are escaped.
      parseMd "<div class=\"a&b\">\n  one & two\n</div>\n\n" `shouldBe` "<div><div class=\"a&amp;b\">\n  one &amp; two\n</div></div>"

    it "parses to horizontal rule" $ do
      parseMd "foobar\n\n---\n\nfoobar\n\n" `shouldBe` "<div><p>foobar</p><hr /><p>foobar</p></div>"
      parseMd "foobar\n\n- -  - \n\nfoobar\n\n" `shouldBe` "<div><p>foobar</p><hr /><p>foobar</p></div>"
      parseMd "foobar\n\n___\n\n***\n\n" `shouldBe` "<div><p>foobar</p><hr /><hr /></div>"

    it "parses to simple list" $ do
      parseMd "- one\n- two\n- three\n\n" `shouldBe` "<div><ul><li>one</li><li>two</li><li>three</li></ul></div>"
      parseMd "+ one\n+ **two**\n+ three\n\n" `shouldBe` "<div><ul><li>one</li><li><strong>two</strong></li><li>three</li></ul></div>"
      parseMd "* one\n* **two**\n* three\n\n" `shouldBe` "<div><ul><li>one</li><li><strong>two</strong></li><li>three</li></ul></div>"

    it "parses to list with multiple lines" $ do
      parseMd "- one\ntwo, three\n- four\n  and\n- five\n\n" `shouldBe` "<div><ul><li>one two, three</li><li>four and</li><li>five</li></ul></div>"

    it "parses to list with paragraphs" $ do
      parseMd "prev para\n\n- one\n\n- two\n\nfollowing para\n\n" `shouldBe` "<div><p>prev para</p><ul><li><p>one</p></li><li><p>two</p></li></ul><p>following para</p></div>"
      parseMd "- one\ntwo\n\n- three\n  four\n\n" `shouldBe` "<div><ul><li><p>one two</p></li><li><p>three four</p></li></ul></div>"
      parseMd "-   one\n\n    two\n\n    three\n\n-   four\n\n    five\n\n" `shouldBe` "<div><ul><li><p>one</p><p>two</p><p>three</p></li><li><p>four</p><p>five</p></li></ul></div>"
      parseMd "- one\n  two\n\n  three\n  four\n\n- five\n  six\n\n" `shouldBe` "<div><ul><li><p>one two</p><p>three four</p></li><li><p>five six</p></li></ul></div>"

    it "parses to list with different levels" $ do
      parseMd "- one\n- two\n    - three\n        - four\n    - five\n- six\n    - seven\n\n" `shouldBe` "<div><ul><li>one</li><li>two<ul><li>three<ul><li>four</li></ul></li><li>five</li></ul></li><li>six<ul><li>seven</li></ul></li></ul></div>"
      parseMd "- one\n\n- two\n\n  three\n\n    - four\n\n" `shouldBe` "<div><ul><li><p>one</p></li><li><p>two</p><p>three</p><ul><li><p>four</p></li></ul></li></ul></div>"

    it "parses to code blocks" $ do
      parseMd "```\none\ntwo  & three\n\nfour&quot;\n```\n\n" `shouldBe` "<div><pre><code>one\ntwo  &amp; three\n\nfour&amp;quot;</code></pre></div>"
      parseMd "```\n**one** and `two`\n```\n\n" `shouldBe` "<div><pre><code>**one** and `two`</code></pre></div>"

    it "parses to block quotes" $ do
      parseMd "prev para\n\n> aaa\n> bbb\n\nfollowing para\n\n" `shouldBe` "<div><p>prev para</p><blockquote><p>aaa bbb</p></blockquote><p>following para</p></div>"
      parseMd "> aaa\nbbb\n\n" `shouldBe` "<div><blockquote><p>aaa bbb</p></blockquote></div>"

    it "parses to block quote with multiple paragraphs" $ do
      parseMd ">one\n>two\n>\n>three\n>\n>four\n>five\n\nfollowing para\n\n" `shouldBe` "<div><blockquote><p>one two</p><p>three</p><p>four five</p></blockquote><p>following para</p></div>"

    -- To parse blocks, space is necessary after '>'
    it "parses to block quote contains other kinds of blocks" $ do
      -- head and list
      parseMd "> # head1\n>\n> - one\n> - two\n>     - three\n\n" `shouldBe` "<div><blockquote><h1>head1</h1><ul><li>one</li><li>two<ul><li>three</li></ul></li></ul></blockquote></div>"
      -- code blocks
      parseMd "> ```\n> code1\n> code2\n> ```\n>\n> ---\n>\n> <div>html content</div>\n\n" `shouldBe` "<div><blockquote><pre><code>code1\ncode2</code></pre><hr /><div>html content</div></blockquote></div>"
      -- html blocks
      parseMd "> <div class=\"a&b\">\n>   one & two\n> </div>\n\n" `shouldBe` "<div><blockquote><div class=\"a&amp;b\">\n  one &amp; two\n</div></blockquote></div>"
      -- inner blockquotes
      parseMd "> one\n>\n> > two\n>\n> three\n\n" `shouldBe` "<div><blockquote><p>one</p><blockquote><p>two</p></blockquote><p>three</p></blockquote></div>"
      parseMd "> one\n>\n> oneone\n>\n> > # two\n> >\n> > twotwo\n\n" `shouldBe` "<div><blockquote><p>one</p><p>oneone</p><blockquote><h1>two</h1><p>twotwo</p></blockquote></blockquote></div>"

    it "parses to strong" $ do
      parseMd "this is **strong**\n\n" `shouldBe` "<div><p>this is <strong>strong</strong></p></div>"
      parseMd "this is **also\nstrong**\n\n" `shouldBe` "<div><p>this is <strong>also strong</strong></p></div>"
      parseMd "this is **str\\*\\*ong**\n\n" `shouldBe` "<div><p>this is <strong>str**ong</strong></p></div>"

    it "parses to emphasis" $ do
      parseMd "this is *emphasis*\n\n" `shouldBe` "<div><p>this is <em>emphasis</em></p></div>"
      parseMd "this is *e\\*m*\n\n" `shouldBe` "<div><p>this is <em>e*m</em></p></div>"

    it "parses to inline link" $ do
      parseMd "this is [inline link](http://foo.com \"inline-link\").\n\n" `shouldBe` "<div><p>this is <a href=\"http://foo.com\" title=\"inline-link\">inline link</a>.</p></div>"

    it "parses to reference link" $ do
      parseMd "this is [reference link][1].\n\n[1]: http://foo.com\n\n" `shouldBe` "<div><p>this is <a href=\"http://foo.com\">reference link</a>.</p></div>"
      parseMd "this is [reference link][1].\n\n[1]: http://foo.com \"link title\"\n\n" `shouldBe` "<div><p>this is <a href=\"http://foo.com\" title=\"link title\">reference link</a>.</p></div>"

    it "parses to multiple reference links" $ do
      parseMd "[one][1], [two][2], [three][3]\n\n[1]: http://one.com\n[2]: http://two.com\n[3]: http://three.com\n\n"
      `shouldBe`
      "<div><p><a href=\"http://one.com\">one</a>, <a href=\"http://two.com\">two</a>, <a href=\"http://three.com\">three</a></p></div>"

    it "parses to linebreak and softbreak in paragraph" $ do
      parseMd "Before break  \nafter break\nand just space\n\n"
      `shouldBe`
      "<div><p>Before break<br />after break and just space</p></div>"

    it "parses incomplete strong symbol to just chars" $ do
      parseMd "**abc\n\n" `shouldBe` "<div><p>**abc</p></div>"
      parseMd "abc**\n\n" `shouldBe` "<div><p>abc**</p></div>"
      parseMd "**abc\n\n**\n\n" `shouldBe` "<div><p>**abc</p><p>**</p></div>"

    it "parses inline codes" $ do
      parseMd "one ` two   three `  four\n\n" `shouldBe` "<div><p>one <code>two   three</code> four</p></div>"
      parseMd "` one & two &amp; three `\n\n" `shouldBe` "<div><p><code>one &amp; two &amp;amp; three</code></p></div>"
      parseMd "``` `two` three `four` ```\n\n" `shouldBe` "<div><p><code>`two` three `four`</code></p></div>"

    it "parses html inline elements and just show original text" $ do
      parseMd "this is framed by <span class=\"cl1\">span</span> tag.\n\n"
      `shouldBe`
      "<div><p>this is framed by <span class=\"cl1\">span</span> tag.</p></div>"

    it "accepts markdown literals inside html inline elements" $ do
      parseMd "this is framed by <span class=\"cl1\">not span but **span**</span> tag.\n\n"
      `shouldBe`
      "<div><p>this is framed by <span class=\"cl1\">not span but <strong>span</strong></span> tag.</p></div>"

    it "escapes markdown literals" $ do
      parseMd "Is this \\*\\*escaped?\\*\\*.\n\n" `shouldBe` "<div><p>Is this **escaped?**.</p></div>"
      parseMd "Is this <span>\\*\\*escaped?\\*\\*.</span>\n\n" `shouldBe` "<div><p>Is this <span>**escaped?**.</span></p></div>"

    it "escapes html" $ do
      parseMd "Is this escaped? \"5 > 2 && 5 < 2\"\n\n" `shouldBe` "<div><p>Is this escaped? &quot;5 &gt; 2 &amp;&amp; 5 &lt; 2&quot;</p></div>"
      parseMd "This is already escaped, &amp;, &lt;, &gt;, & &quot;\n\n" `shouldBe` "<div><p>This is already escaped, &amp;, &lt;, &gt;, &amp; &quot;</p></div>"

  describe "parseMdFile" $ do
    it "parses complex markdown" $ do
      actual <- parseMdFileFormat "samples/sample.md"
      expect <- readFile "samples/sample-expect.html"
      actual `shouldBe` expect
