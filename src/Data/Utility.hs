{-# LANGUAGE NoImplicitPrelude #-}

module Data.Utility where

import qualified Data.Aeson as DA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.IP as IP
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Network.Socket as Socket
import Relude
import qualified Web.Scotty as Scotty
import qualified Prelude

lbsToSText :: BL.ByteString -> Text
lbsToSText = T.decodeUtf8 . B.concat . BL.toChunks

makeResponse :: DA.ToJSON a => a -> Scotty.ActionM ()
makeResponse x = do
  Scotty.addHeader "Content-Type" "application/json"
  Scotty.text . toLText . lbsToSText $ DA.encode x

getHeader :: LText -> [(LText, LText)] -> Text
getHeader name headers = TL.toStrict $ snd x
  where
    x = Prelude.head $ filter (\(k, _) -> k == name) headers

saToIP :: Socket.SockAddr -> Text
saToIP sa = case sa of
  Socket.SockAddrInet _ ha -> show (IP.IPv4 $ IP.fromHostAddress ha)
  Socket.SockAddrInet6 _ _ ha _ -> show (IP.IPv6 $ IP.fromHostAddress6 ha)
  _ -> error "Couldn't get IP!"
