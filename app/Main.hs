{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Aeson as DA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Utility as U
import qualified Network.Wai as WAI
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Relude
import qualified Service.FileMetadata as SFM
import qualified Service.RequestHeaderParser as SRHP
import qualified Web.Scotty as Scotty

main :: IO ()
main = Scotty.scotty 3030 $ do
  Scotty.middleware logStdoutDev
  Scotty.get "/" $ do
    Scotty.html "<h1>Hello world!</h1>"

  Scotty.get "/api/whoami" $ do
    h <- Scotty.headers
    sa <- WAI.remoteHost <$> Scotty.request
    U.makeResponse $
      SRHP.Response
        (U.socketAddressToIP sa)
        (U.getHeader "Accept-Language" h)
        (U.getHeader "User-Agent" h)

  Scotty.get "/filemetadata" $ do
    Scotty.file "filemetadata.html"

  Scotty.post "/api/fileanalyse" $ do
    U.makeResponse $ SFM.Response "" "" 10
