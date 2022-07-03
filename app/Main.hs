{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Aeson as DA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.IP as IP
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Utility as U
import qualified Network.Socket as Socket
import qualified Network.Wai as WAI
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Relude
import qualified Service.RequestHeaderParser as SHRP
import qualified Web.Scotty as Scotty

saToIP :: Socket.SockAddr -> Text
saToIP sa = case sa of
  Socket.SockAddrInet _ ha -> show (IP.IPv4 $ IP.fromHostAddress ha)
  Socket.SockAddrInet6 _ _ ha _ -> show (IP.IPv6 $ IP.fromHostAddress6 ha)
  _ -> error "Couldn't get IP!"

main :: IO ()
main = Scotty.scotty 3030 $ do
  Scotty.middleware logStdoutDev
  Scotty.get "/" $ do
    Scotty.html "<h1>Index page</h1>"

  Scotty.get "/api/whoami" $ do
    h <- Scotty.headers
    sa <- WAI.remoteHost <$> Scotty.request
    U.makeResponse $ SHRP.Response (saToIP sa) (U.getHeader "Accept-Language" h) (U.getHeader "User-Agent" h)
