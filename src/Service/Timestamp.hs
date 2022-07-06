{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.Timestamp where

import qualified Data.Aeson as DA
import Data.Aeson.Types
  ( KeyValue ((.=)),
    ToJSON (toJSON),
  )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as DT
import qualified Data.Time.Clock.System as DTCS
import qualified Data.Utility as U
import Relude
import qualified Prelude

data Response = Response
  { unix :: !Integer,
    utc :: !Text
  }
  deriving (Show, Generic, DA.ToJSON)

-- | This will parse unix time (epoch) and locale time
readTime :: Text -> Maybe DT.UTCTime
readTime t =
  case localeTime t of
    Nothing -> unixTime t
    Just ut -> Just ut
  where
    it = T.unpack t
    localeTime x = DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d %H:%M:%S" it :: Maybe DT.UTCTime
    unixTime y = DT.parseTimeM True DT.defaultTimeLocale "%s" it

utcAsUnix :: DT.UTCTime -> Integer
utcAsUnix = Prelude.read . DT.formatTime DT.defaultTimeLocale "%s"

utcAsDefaultLocale :: DT.UTCTime -> Text
utcAsDefaultLocale = T.pack . DT.formatTime DT.defaultTimeLocale "%a, %d %b %Y %T %Z"
