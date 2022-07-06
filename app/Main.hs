{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified App
import qualified Data.AppState as DAS
import qualified Data.Default.Class as DDC
import qualified Data.Utility as U
import Relude
import qualified Web.Scotty.Trans as Scotty

main :: IO ()
main = do
  sync <- newTVarIO DDC.def
  let runActionToIO m = runReaderT (DAS.runWebM m) sync
  Scotty.scottyT U.port runActionToIO App.app
