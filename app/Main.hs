{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Aeson as DA
import Data.Aeson.Types
  ( KeyValue ((.=)),
    ToJSON (toJSON),
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Relude
import qualified Web.Scotty as Scotty

main :: IO ()
main = Scotty.scotty 3030 $ do
  Scotty.get "/" $ do
    Scotty.html "<h1>Index page</h1>"
