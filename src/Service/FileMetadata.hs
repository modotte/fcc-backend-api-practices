{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.FileMetadata where

import qualified Data.Aeson as DA
import Data.Aeson.Types
  ( KeyValue ((.=)),
    ToJSON (toJSON),
  )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai.Request as Request
import Relude

data Response = Response
  { name :: Text,
    rType :: Text,
    size :: Int
  }
  deriving (Show, Generic)

instance DA.ToJSON Response where
  toJSON (Response _name _rType _size) =
    DA.object
      [ "name" .= _name,
        "type" .= _rType,
        "size" .= _size
      ]
