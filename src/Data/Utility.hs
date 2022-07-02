{-# LANGUAGE NoImplicitPrelude #-}

module Data.Utility where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Relude
import qualified Web.Scotty as Scotty

lbsToSText :: BL.ByteString -> Text
lbsToSText = T.decodeUtf8 . B.concat . BL.toChunks

makeResponse :: BL.ByteString -> Scotty.ActionM ()
makeResponse x = do
  Scotty.addHeader "Content-Type" "application/json"
  Scotty.text . toLText $ lbsToSText x
