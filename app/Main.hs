{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Aeson as DA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Utility as U
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Relude
import qualified Service.RequestHeaderParser as SHRP
import qualified Web.Scotty as Scotty

main :: IO ()
main = Scotty.scotty 3030 $ do
  Scotty.middleware logStdoutDev
  Scotty.get "/" $ do
    Scotty.html "<h1>Index page</h1>"

  Scotty.get "/api/whoami" $ do
    h <- Scotty.headers
    U.makeResponse $ SHRP.Response "Text" "Text" (U.getUserAgent h)
