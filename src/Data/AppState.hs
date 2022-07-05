{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.AppState where

import Data.Default.Class (Default (..))
import Data.Functor ((<&>))
import qualified Data.HashMap.Lazy as HML
import Relude

data AppState = AppState {urlCounter :: Int, urls :: HashMap Int Text}

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
