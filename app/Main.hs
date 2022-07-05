{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.AppState as DAS
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class (Default (..))
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Utility as U
import qualified Network.Wai as WAI
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Relude
import qualified Service.FileMetadata as SFM
import qualified Service.RequestHeaderParser as SRHP
import qualified Service.UrlShortener as US
import qualified Web.Scotty.Trans as Scotty
import qualified Prelude

app :: Scotty.ScottyT LText DAS.WebM ()
app = do
  Scotty.middleware logStdoutDev
  Scotty.get "/" $ do
    Scotty.file "index.html"

  Scotty.get "/service/headerparser" $
    Scotty.file
      "headerparser.html"

  Scotty.get "/api/whoami" $ do
    h <- Scotty.headers
    sa <- WAI.remoteHost <$> Scotty.request
    U.makeResponse $
      SRHP.Response
        (U.socketAddressToIP sa)
        (U.getHeader "Accept-Language" h)
        (U.getHeader "User-Agent" h)

  Scotty.get "/service/filemetadata" $ do
    Scotty.file "filemetadata.html"

  -- FIXME: Handle empty file!!
  Scotty.post "/api/fileanalyse" $ do
    fs <- Scotty.files
    let fi = (snd . Prelude.head) fs
    U.makeResponse $ SFM.getFileMetadata fi

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

main :: IO ()
main = do
  sync <- newTVarIO def
  let runActionToIO m = runReaderT (DAS.runWebM m) sync
  Scotty.scottyT U.port runActionToIO app
