module Text.Md.MdParserSpec (
  main,
  spec,
) where

import Text.Md.MdParser
import Test.Hspec

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

    it "parses to strong" $ do
      parseMarkdown "this is **strong**\n\n" `shouldBe` "<div><p>this is <strong>strong</strong></p></div>"
      parseMarkdown "this is **also\nstrong**\n\n" `shouldBe` "<div><p>this is <strong>also strong</strong></p></div>"

    it "parses to linebreak and softbreak in paragraph" $ do
      parseMarkdown "Before break  \nafter break\nand just space\n\n"
      `shouldBe`
      "<div><p>Before break<br />after break and just space</p></div>"

    it "parses incomplete strong symbol to just chars" $ do
      parseMarkdown "**abc\n\n" `shouldBe` "<div><p>**abc</p></div>"
      parseMarkdown "abc**\n\n" `shouldBe` "<div><p>abc**</p></div>"
      parseMarkdown "**abc\n\n**\n\n" `shouldBe` "<div><p>**abc</p><p>**</p></div>"

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
