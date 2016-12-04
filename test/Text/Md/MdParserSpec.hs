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
  describe "parseMarkdown" $ do
    it "parses to paragraph" $
      parseMarkdown "foobar\n\n"
      `shouldBe`
      "<div><p>foobar</p></div>"

    it "parses to header" $ do
      parseMarkdown "# header1\n\npara1\n\n##   header2\n\n### header3\n\n"
      `shouldBe`
      "<div><h1>header1</h1><p>para1</p><h2>header2</h2><h3>header3</h3></div>"

    it "parses to horizontal rule" $ do
      parseMarkdown "foobar\n\n---\n\nfoobar\n\n" `shouldBe` "<div><p>foobar</p><hr /><p>foobar</p></div>"
      parseMarkdown "foobar\n\n- -  - \n\nfoobar\n\n" `shouldBe` "<div><p>foobar</p><hr /><p>foobar</p></div>"
      parseMarkdown "foobar\n\n___\n\n***\n\n" `shouldBe` "<div><p>foobar</p><hr /><hr /></div>"

    it "parses to simple list" $ do
      parseMarkdown "- one\n- two\n- three\n\n" `shouldBe` "<div><ul><li>one</li><li>two</li><li>three</li></ul></div>"
      parseMarkdown "+ one\n+ **two**\n+ three\n\n" `shouldBe` "<div><ul><li>one</li><li><strong>two</strong></li><li>three</li></ul></div>"
      parseMarkdown "* one\n* **two**\n* three\n\n" `shouldBe` "<div><ul><li>one</li><li><strong>two</strong></li><li>three</li></ul></div>"

    -- FIXME: Should ignore soft break or first spaces the following lines in paragraphs
    it "parses to list with multiple lines" $ do
      parseMarkdown "- one\ntwo, three\n- four\n  and\n- five\n\n" `shouldBe` "<div><ul><li>one two, three</li><li>four  and</li><li>five</li></ul></div>"

    -- FIXME: Should ignore soft break or first spaces the following lines in paragraphs
    it "parses to list with paragraphs" $ do
      parseMarkdown "prev para\n\n- one\n\n- two\n\nfollowing para\n\n" `shouldBe` "<div><p>prev para</p><ul><li><p>one</p></li><li><p>two</p></li></ul><p>following para</p></div>"
      parseMarkdown "- one\ntwo\n\n- three\n  four\n\n" `shouldBe` "<div><ul><li><p>one two</p></li><li><p>three  four</p></li></ul></div>"
      parseMarkdown "-   one\n\n    two\n\n    three\n\n-   four\n\n    five\n\n" `shouldBe` "<div><ul><li><p>one</p><p>two</p><p>three</p></li><li><p>four</p><p>five</p></li></ul></div>"

    it "parses to strong" $ do
      parseMarkdown "this is **strong**\n\n" `shouldBe` "<div><p>this is <strong>strong</strong></p></div>"
      parseMarkdown "this is **also\nstrong**\n\n" `shouldBe` "<div><p>this is <strong>also strong</strong></p></div>"

    it "parses to inline link" $ do
      parseMarkdown "this is [inline link](http://foo.com \"inline-link\").\n\n" `shouldBe` "<div><p>this is <a href=\"http://foo.com\" title=\"inline-link\">inline link</a>.</p></div>"

    it "parses to reference link" $ do
      parseMarkdown "this is [reference link][1].\n\n[1]: http://foo.com\n\n" `shouldBe` "<div><p>this is <a href=\"http://foo.com\">reference link</a>.</p></div>"
      parseMarkdown "this is [reference link][1].\n\n[1]: http://foo.com \"link title\"\n\n" `shouldBe` "<div><p>this is <a href=\"http://foo.com\" title=\"link title\">reference link</a>.</p></div>"

    it "parses to multiple reference links" $ do
      parseMarkdown "[one][1], [two][2], [three][3]\n\n[1]: http://one.com\n[2]: http://two.com\n[3]: http://three.com\n\n"
      `shouldBe`
      "<div><p><a href=\"http://one.com\">one</a>, <a href=\"http://two.com\">two</a>, <a href=\"http://three.com\">three</a></p></div>"

    it "parses to linebreak and softbreak in paragraph" $ do
      parseMarkdown "Before break  \nafter break\nand just space\n\n"
      `shouldBe`
      "<div><p>Before break<br />after break and just space</p></div>"

    it "parses incomplete strong symbol to just chars" $ do
      parseMarkdown "**abc\n\n" `shouldBe` "<div><p>**abc</p></div>"
      parseMarkdown "abc**\n\n" `shouldBe` "<div><p>abc**</p></div>"
      parseMarkdown "**abc\n\n**\n\n" `shouldBe` "<div><p>**abc</p><p>**</p></div>"

    it "parses inline codes" $ do
      parseMarkdown "one ` two   three `  four\n\n" `shouldBe` "<div><p>one <code>two   three</code> four</p></div>"
      parseMarkdown "` one & two &amp; three `\n\n" `shouldBe` "<div><p><code>one &amp; two &amp; three</code></p></div>"
      parseMarkdown "``` `two` three `four` ```\n\n" `shouldBe` "<div><p><code>`two` three `four`</code></p></div>"

    it "parse html inline elements and just show original text" $ do
      parseMarkdown "this is framed by <span class=\"cl1\">span</span> tag.\n\n"
      `shouldBe`
      "<div><p>this is framed by <span class=\"cl1\">span</span> tag.</p></div>"

    it "accepts markdown literals inside html inline elements" $ do
      parseMarkdown "this is framed by <span class=\"cl1\">not span but **span**</span> tag.\n\n"
      `shouldBe`
      "<div><p>this is framed by <span class=\"cl1\">not span but <strong>span</strong></span> tag.</p></div>"

    it "escape markdown literals" $ do
      parseMarkdown "Is this \\*\\*escaped?\\*\\*.\n\n" `shouldBe` "<div><p>Is this **escaped?**.</p></div>"
      parseMarkdown "Is this <span>\\*\\*escaped?\\*\\*.</span>\n\n" `shouldBe` "<div><p>Is this <span>**escaped?**.</span></p></div>"

    it "escape html" $ do
      parseMarkdown "Is this escaped? \"5 > 2 && 5 < 2\"\n\n" `shouldBe` "<div><p>Is this escaped? &quot;5 &gt; 2 &amp;&amp; 5 &lt; 2&quot;</p></div>"
      parseMarkdown "This is already escaped, &amp;, &lt;, &gt;, & &quot;\n\n" `shouldBe` "<div><p>This is already escaped, &amp;, &lt;, &gt;, &amp; &quot;</p></div>"

    -- should parse markdown literals in html block elements? -> no, for now
    -- should perform html escaping
    it "parse html block elements to blocks without framing by 'p' tag" $ do
      parseMarkdown "line1\n\n<div id=\"header\">**line2**</div>\n\nline3\n\n" `shouldBe` "<div><p>line1</p><div id=\"header\">**line2**</div><p>line3</p></div>"
      parseMarkdown "line1\n\n<div><ul><li>list1</li><li>list2</li>\n\n</ul></div>\n\nline3\n\n" `shouldBe` "<div><p>line1</p><div><ul><li>list1</li><li>list2</li>\n\n</ul></div><p>line3</p></div>"
