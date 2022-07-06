{-# LANGUAGE DeriveGeneric #-}

module Data.Utility where

import qualified Data.Aeson as DA
import Data.Aeson.Types
  ( KeyValue ((.=)),
    ToJSON (toJSON),
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.IP as IP
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Network.Socket as Socket
import Relude
import qualified Web.Scotty.Trans as Scotty

port :: Int
port = 3030

newtype ErrorResponse = ErrorResponse {errorResp :: Text} deriving (Show, Generic)

instance DA.ToJSON ErrorResponse where
  toJSON (ErrorResponse _errorResp) = DA.object ["error" .= _errorResp]

lbsToSText :: BL.ByteString -> Text
lbsToSText = T.decodeUtf8 . B.concat . BL.toChunks

makeResponse :: (Monad m, Scotty.ScottyError e, DA.ToJSON a) => a -> Scotty.ActionT e m ()
makeResponse x = do
  Scotty.addHeader "Content-Type" "application/json"
  Scotty.text . toLText . lbsToSText $ DA.encode x

getHeader :: LText -> [(LText, LText)] -> Either Text Text
getHeader name headers =
  case x of
    Nothing -> Left $ TL.toStrict name <> " header not found!"
    Just h -> Right $ TL.toStrict $ snd h
  where
    x = viaNonEmpty head $ filter (\(k, _) -> k == name) headers

socketAddressToIP :: Socket.SockAddr -> Text
socketAddressToIP sa = case sa of
  Socket.SockAddrInet _ ha -> show (IP.IPv4 $ IP.fromHostAddress ha)
  Socket.SockAddrInet6 _ _ ha _ -> show (IP.IPv6 $ IP.fromHostAddress6 ha)
  _ -> error "Couldn't get IP!"
