import qualified Data.Text.Lazy as TL
import Data.Utility as U
import Relude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "get user-agent header value" $
        let ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.115 Safari/537.36"
         in U.getHeader "User-Agent" [("User-Agent", ua)] @?= Right (TL.toStrict ua),
      testCase "get accept-language header value" $
        let al = "en-US,en;q=0.9"
         in U.getHeader "Accept-Language" [("Accept-Language", al)] @?= Right (TL.toStrict al)
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property tests"
    []

allTests :: TestTree
allTests = testGroup "All test" [unitTests, propertyTests]

main :: IO ()
main = do
  defaultMain allTests
