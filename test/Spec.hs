import qualified Data.Text.Lazy as TL
import qualified Data.Utility as U
import Relude
import qualified Service.Timestamp as ST
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

hspecSuite :: Spec
hspecSuite = do
  describe "U.getHeader: gets client header" $ do
    it "gets User-Agent client header" $ do
      let ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.115 Safari/537.36"
       in U.getHeader "User-Agent" [("User-Agent", ua)] `shouldBe` Right (TL.toStrict ua)

    it "gets Accept-Language client header" $ do
      let al = "en-US,en;q=0.9"
       in U.getHeader "Accept-Language" [("Accept-Language", al), ("User-Agent", "blahlalala")] `shouldBe` Right (TL.toStrict al)

  describe "ST.readTime: try to parse time as text and converts to unix or utc time based on accepted formats" $ do
    it "parse invalid unix format" $ do
      ST.readTime "1451001600.2323(@(#U&@#" `shouldBe` Nothing

    it "parse invalid date-time format" $ do
      ST.readTime "2015-08-02 20:30:02 GMT" `shouldBe` Nothing

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property tests"
    []

main :: IO ()
main = do
  hspecTests <- testSpec "hspec tests" hspecSuite
  defaultMain $
    testGroup "All tests" [hspecTests, propertyTests]
