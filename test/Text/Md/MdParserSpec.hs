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
