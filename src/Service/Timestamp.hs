{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Service.Timestamp where

import qualified Data.Aeson as DA
import qualified Data.Text as T
import qualified Data.Time as DT
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
  case localeTime it of
    Nothing -> unixTime it
    Just ut -> Just ut
  where
    it = T.unpack t
    localeTime x = DT.parseTimeM True DT.defaultTimeLocale "%Y-%-m-%-d %H:%M:%S" x :: Maybe DT.UTCTime
    unixTime y = DT.parseTimeM True DT.defaultTimeLocale "%s" y :: Maybe DT.UTCTime

utcAsUnix :: DT.UTCTime -> Integer
utcAsUnix = Prelude.read . DT.formatTime DT.defaultTimeLocale "%s"

utcAsDefaultLocale :: DT.UTCTime -> Text
utcAsDefaultLocale = T.pack . DT.formatTime DT.defaultTimeLocale "%a, %d %b %Y %T %Z"
