module Herd.Console.Effect where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Herd.Console.Remote (SPParams_(..))
import Servant.PureScript.Ajax (AjaxError)
import Servant.PureScript.Ajax as Ajax
import Servant.PureScript.Settings (SPSettings_, defaultSettings)

type ConsoleAjaxSettings = SPSettings_ SPParams_

defaultConsoleSettings :: ConsoleAjaxSettings
defaultConsoleSettings = defaultSettings $ SPParams_ { baseURL : "http://localhost:8081/" }

newtype ConsoleAff a =
  ConsoleAff (ReaderT ConsoleAjaxSettings (ExceptT AjaxError Aff) a)

derive instance newtypeConsoleAff :: Newtype (ConsoleAff a) _
derive newtype instance functorConsoleAff :: Functor ConsoleAff
derive newtype instance applyConsoleAff :: Apply ConsoleAff
derive newtype instance applicativeConsoleAff :: Applicative ConsoleAff
derive newtype instance bindConsoleAff :: Bind ConsoleAff
derive newtype instance monadConsoleAff :: Monad ConsoleAff
derive newtype instance monadAskConsoleAff :: MonadAsk (SPSettings_ SPParams_) ConsoleAff
derive newtype instance monadThrowConsoleAff :: MonadThrow AjaxError ConsoleAff
derive newtype instance monadErrorConsoleAff :: MonadError AjaxError ConsoleAff
derive newtype instance monadEffectConsoleAff :: MonadEffect ConsoleAff
derive newtype instance monadAffConsoleAff :: MonadAff ConsoleAff

runConsoleAff :: forall a. ConsoleAjaxSettings -> ConsoleAff a -> Aff a
runConsoleAff settings (ConsoleAff fa) = do
  res <- runExceptT $ runReaderT fa settings
  case res of
    Left err -> throwError $ error (Ajax.errorToString err)
    Right v -> pure v