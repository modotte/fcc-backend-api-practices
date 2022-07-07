import qualified Data.Fixed as DF
import qualified Data.Text.Lazy as TL
import qualified Data.Time as DT
import qualified Data.Utility as U
import Relude
import qualified Service.Timestamp as ST
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

mkUTCTime ::
  (Integer, Int, Int) ->
  (Int, Int, DF.Pico) ->
  DT.UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  DT.UTCTime
    (DT.fromGregorian year mon day)
    (DT.timeOfDayToTime (DT.TimeOfDay hour min sec))

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
      ST.readTime "2015-08-02 2000:30:02" `shouldBe` Nothing

    it "parse valid utc timelocale format: 2022-08-14 00:00:20" $ do
      ST.readTime "2022-08-14 00:00:20" `shouldBe` Just (mkUTCTime (2022, 08, 14) (00, 00, 20))

    it "parse valid unix format: 1451001600" $ do
      ST.readTime "1451001600" `shouldBe` Just (mkUTCTime (2015, 12, 25) (00, 00, 00))

    it "convert utc time to unix integer" $ do
      ST.utcAsUnix (mkUTCTime (2022, 06, 25) (00, 00, 00)) `shouldBe` Just 1656115200

    it "convert utc time to default locale time as text" $ do
      ST.utcAsDefaultLocale (mkUTCTime (1995, 02, 18) (00, 23, 00)) `shouldBe` "Sat, 18 Feb 1995 00:23:00 UTC"

main :: IO ()
main = do
  hspecTests <- testSpec "hspec tests" hspecSuite
  defaultMain $
    testGroup "All tests" [hspecTests]
