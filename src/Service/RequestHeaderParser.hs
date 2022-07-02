{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.RequestHeaderParser where

import qualified Data.Aeson as DA
import Data.Aeson.Types
  ( KeyValue ((.=)),
    ToJSON (toJSON),
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Relude

data Response = Response
  { ipAddress :: Text,
    language :: Text,
    software :: Text
  }
  deriving (Show, Generic)

instance DA.ToJSON Response where
  toJSON (Response ipAddress language software) =
    DA.object
      [ "ipaddress" .= ipAddress,
        "language" .= language,
        "software" .= software
      ]
