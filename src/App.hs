{-# LANGUAGE ScopedTypeVariables #-}

module App (app) where

import qualified Data.AppState as DAS
import qualified Data.HashMap.Lazy as HML
import qualified Data.Time as DT
import qualified Data.Utility as U
import qualified Network.Wai as WAI
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Relude
import qualified Service.FileMetadata as SFM
import qualified Service.RequestHeaderParser as SRHP
import qualified Service.Timestamp as ST
import qualified Service.UrlShortener as US
import qualified Web.Scotty.Trans as Scotty
import qualified Prelude

app :: Scotty.ScottyT LText DAS.WebM ()
app = do
  Scotty.middleware logStdoutDev
  Scotty.get "/" $ do
    Scotty.file "index.html"

  Scotty.get "/service/headerparser" $
    Scotty.file "headerparser.html"

  Scotty.get "/api/whoami" $ do
    h <- Scotty.headers
    sa <- WAI.remoteHost <$> Scotty.request
    U.makeResponse $
      SRHP.Response
        (U.socketAddressToIP sa)
        (U.getHeader "Accept-Language" h)
        (U.getHeader "User-Agent" h)

  Scotty.get "/service/timestamp" $ do
    Scotty.file "timestamp.html"

  Scotty.get "/api/date/:date" $ do
    date :: Text <- Scotty.param "date"
    if date == ""
      then do
        ct <- liftIO DT.getCurrentTime
        U.makeResponse $ ST.Response (ST.utcAsUnix ct) (ST.utcAsDefaultLocale ct)
      else case ST.readTime date of
        Nothing -> U.makeResponse $ U.ErrorResponse "Invalid date format!!! Please try again!! We only accept two time formats: %Y-%-m-%-d %H:%M:%S and %s"
        Just pt ->
          U.makeResponse $
            ST.Response (ST.utcAsUnix pt) (ST.utcAsDefaultLocale pt)

  Scotty.get "/service/filemetadata" $ do
    Scotty.file "filemetadata.html"

  Scotty.post "/api/fileanalyse" $ do
    fs <- Scotty.files
    let fi = snd $ Prelude.head fs
    case SFM.getFileMetadata fi of
      Left err -> U.makeResponse $ U.ErrorResponse err
      Right r -> U.makeResponse r

  Scotty.get "/service/shorturl" $ do
    Scotty.file "shorturl.html"

  Scotty.get "/api/shorturl/:uid" $ do
    uid :: Int <- Scotty.param "uid"
    currentUrls <- DAS.webM $ DAS.get DAS.urls
    case HML.lookup uid currentUrls of
      Nothing ->
        U.makeResponse $ U.ErrorResponse "Short URL doesn't exist! Please add the new origin URL first!"
      Just originUrl -> Scotty.redirect $ toLText originUrl

  Scotty.post "/api/shorturl/" $ do
    originUrl :: Text <- Scotty.param "origin"

    currentUrls <- DAS.webM $ DAS.get DAS.urls

    if US.isOriginUrlExists originUrl currentUrls
      then DAS.webM US.incrementUrlCounter
      else pure ()

    nc <- DAS.webM $ DAS.get DAS.urlCounter
    DAS.webM $ US.addUrl nc originUrl
    U.makeResponse $ US.Response originUrl $ US.shortUrlPath <> show nc
