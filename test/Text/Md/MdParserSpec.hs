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
    it "parses just string to paragraph" $
      parseMarkdown "foobar\n\n" `shouldBe` "<div><p>foobar</p></div>"
