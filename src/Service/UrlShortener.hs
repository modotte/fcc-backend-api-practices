{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.UrlShortener where

import qualified Data.Aeson as DA
import Data.Aeson.Types
  ( KeyValue ((.=)),
    ToJSON (toJSON),
  )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
