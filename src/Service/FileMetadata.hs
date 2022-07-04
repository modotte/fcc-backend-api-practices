{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.FileMetadata where

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
  { name :: Text,
    rType :: Text,
    size :: Integer
  }
  deriving (Show, Generic)

instance DA.ToJSON Response where
  toJSON (Response _name _rType _size) =
    DA.object
      [ "name" .= _name,
        "type" .= _rType,
        "size" .= _size
      ]

getFileMetadata :: WAIP.FileInfo BL.ByteString -> Response
getFileMetadata fi =
  Response
    (T.decodeUtf8 $ WAIP.fileName fi)
    (T.decodeUtf8 $ WAIP.fileContentType fi)
    (toInteger $ (BL.length . WAIP.fileContent) fi)
