{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.UrlShortener where

import qualified Data.Aeson as DA
import Data.Aeson.Types
  ( KeyValue ((.=)),
    ToJSON (toJSON),
  )
import qualified Data.AppState as DAS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Utility as U
import qualified Network.Wai.Parse as WAIP
import qualified Network.Wai.Request as Request
import Relude

data Response = Response
  { originalUrl :: Text,
    shortUrl :: Text
  }

instance DA.ToJSON Response where
  toJSON (Response _originalUrl _shortUrl) =
    DA.object
      [ "original_url" .= _originalUrl,
        "short_url" .= _shortUrl
      ]

shortUrlPath :: Text
shortUrlPath = "http://127.0.0.1:" <> show U.port <> "/api/shorturl/"

incrementUrlCounter :: DAS.WebM ()
incrementUrlCounter = DAS.modify $ \x -> x {DAS.urlCounter = DAS.urlCounter x + 1}

addUrl :: Int -> Text -> DAS.WebM ()
addUrl shortUrl originUrl = DAS.modify $ \x -> x {DAS.urls = HML.insert shortUrl originUrl $ DAS.urls x}

isOriginUrlExists :: Text -> HashMap Int Text -> Bool
isOriginUrlExists originUrl _urls = isNothing . find (== originUrl) $ HML.elems _urls