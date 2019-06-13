module Herd.Console.Effect where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (error)
import Herd.Console.Remote (SPParams_(..))
import Servant.PureScript.Ajax (AjaxError)
import Servant.PureScript.Ajax as Ajax
import Servant.PureScript.Settings (SPSettings_, defaultSettings)

type RemoteSettings = SPSettings_ SPParams_

defaultRemoteSettings :: RemoteSettings
defaultRemoteSettings = defaultSettings $ SPParams_ { baseURL : "http://localhost:8081/" }

newtype RemoteAff a =
  RemoteAff (ReaderT RemoteSettings (ExceptT AjaxError Aff) a)

derive instance newtypeRemoteAff :: Newtype (RemoteAff a) _
derive newtype instance functorRemoteAff :: Functor RemoteAff
derive newtype instance applyRemoteAff :: Apply RemoteAff
derive newtype instance applicativeRemoteAff :: Applicative RemoteAff
derive newtype instance bindRemoteAff :: Bind RemoteAff
derive newtype instance monadRemoteAff :: Monad RemoteAff
derive newtype instance monadAskRemoteAff :: MonadAsk (SPSettings_ SPParams_) RemoteAff
derive newtype instance monadThrowRemoteAff :: MonadThrow AjaxError RemoteAff
derive newtype instance monadErrorRemoteAff :: MonadError AjaxError RemoteAff
derive newtype instance monadEffectRemoteAff :: MonadEffect RemoteAff
derive newtype instance monadAffRemoteAff :: MonadAff RemoteAff

runRemoteAff :: forall a. RemoteSettings -> RemoteAff a -> Aff a
runRemoteAff settings (RemoteAff fa) = do
  res <- runExceptT $ runReaderT fa settings
  case res of
    Left err -> throwError $ error (Ajax.errorToString err)
    Right v -> pure v