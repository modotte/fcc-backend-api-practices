{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Service.Timestamp
  ( Response (..),
    readTime,
    utcAsUnix,
    utcAsDefaultLocale,
  )
where

import qualified Data.Aeson as DA
import qualified Data.Text as T
import qualified Data.Time as DT
import Relude

data Response = Response
  { unix :: !Integer,
    utc :: !Text
  }
  deriving (Show, Generic, DA.ToJSON)

parseAsFormat :: String -> Text -> Maybe DT.UTCTime
parseAsFormat format = DT.parseTimeM True DT.defaultTimeLocale format . T.unpack

-- | This will parse unix time (epoch) and locale time
readTime :: Text -> Maybe DT.UTCTime
readTime t = localeTime <|> unixTime
  where
    localeTime = parseAsFormat "%Y-%-m-%-d %H:%M:%S" t
    unixTime = parseAsFormat "%s" t

utcAsUnix :: DT.UTCTime -> Maybe Integer
utcAsUnix = readMaybe . DT.formatTime DT.defaultTimeLocale "%s"

utcAsDefaultLocale :: DT.UTCTime -> Text
utcAsDefaultLocale = T.pack . DT.formatTime DT.defaultTimeLocale "%a, %d %b %Y %T %Z"
