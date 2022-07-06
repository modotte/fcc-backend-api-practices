{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.RequestHeaderParser where

import qualified Data.Aeson as DA
import Data.Aeson.Types
  ( KeyValue ((.=)),
    ToJSON (toJSON),
  )
import Relude

data Response = Response
  { ipAddress :: Text,
    language :: Text,
    software :: Text
  }
  deriving (Show, Generic)

instance DA.ToJSON Response where
  toJSON (Response _ipAddress _language _software) =
    DA.object
      [ "ipaddress" .= _ipAddress,
        "language" .= _language,
        "software" .= _software
      ]
