{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.Aeson as DA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default.Class (Default (..))
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Utility as U
import qualified Network.Wai as WAI
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Relude
import qualified Service.FileMetadata as SFM
import qualified Service.RequestHeaderParser as SRHP
import qualified Service.UrlShortener as US
import qualified Web.Scotty.Trans as Scotty
import qualified Prelude

newtype AppState = AppState {urls :: HashMap Text Text}

instance Default AppState where
  def = AppState HML.empty

newtype WebM a = WebM {runWebM :: ReaderT (TVar AppState) IO a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

get :: (AppState -> b) -> WebM b
get f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

addUrl :: Text -> Text -> WebM ()
addUrl origin shortUrl = Main.modify $ \x -> x {urls = HML.insert origin shortUrl $ urls x}

app :: Scotty.ScottyT LText WebM ()
app = do
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
    fs <- Scotty.files
    let fi = (snd . Prelude.head) fs
    U.makeResponse $ SFM.getFileMetadata fi

  Scotty.post "/api/shorturl" $ do
    (origin :: Text) <- Scotty.param "origin"
    webM $ addUrl origin "new"
    s <- webM $ Main.get urls
    Scotty.text $ show s

main :: IO ()
main = do
  sync <- newTVarIO def
  let runActionToIO m = runReaderT (runWebM m) sync
  Scotty.scottyT 3030 runActionToIO app
