{-# LANGUAGE NoImplicitPrelude #-}

module Data.Utility where

import qualified Data.Aeson as DA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Relude
import qualified Web.Scotty as Scotty
import qualified Prelude

lbsToSText :: BL.ByteString -> Text
lbsToSText = T.decodeUtf8 . B.concat . BL.toChunks

makeResponse :: DA.ToJSON a => a -> Scotty.ActionM ()
makeResponse x = do
  Scotty.addHeader "Content-Type" "application/json"
  Scotty.text . toLText . lbsToSText $ DA.encode x

getUserAgent :: [(LText, LText)] -> Text
getUserAgent headers = TL.toStrict $ snd x
  where
    x = Prelude.head $ filter (\(k, _) -> k == "User-Agent") headers
