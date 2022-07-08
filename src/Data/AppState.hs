{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.AppState where

import Control.Exception
import qualified Data.Aeson as DA
import Data.Aeson.Types
  ( KeyValue ((.=)),
    ToJSON (toJSON),
  )
import Data.Default.Class (Default (..))
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Utility as U
import Relude

data AppState = AppState {urlCounter :: Int, urls :: HashMap Int Text} deriving (Show, Generic)

instance DA.ToJSON AppState

instance DA.FromJSON AppState

instance Default AppState where
  def =
    AppState 1 $
      HML.fromList [(1, "https://forum.freecodecamp.org/")]

newtype WebM a = WebM {runWebM :: ReaderT (TVar AppState) IO a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

get :: (AppState -> b) -> WebM b
get f = (ask >>= liftIO . readTVarIO) <&> f

modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

-- In this case, we don't care what exactly if the file can't be accessed.
-- We simply use the default AppState if we can't open the file.
load :: WebM (Maybe AppState)
load = do
  d :: Either SomeException LByteString <- liftIO $ try (readFileLBS U.dbName)
  case d of
    Left _ -> pure Nothing
    Right s -> pure (DA.decode s :: Maybe AppState)

save :: AppState -> WebM ()
save = writeFileLBS U.dbName . DA.encode
