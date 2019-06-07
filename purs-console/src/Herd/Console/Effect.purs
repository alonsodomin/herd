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

instance functorConsoleAff :: Functor ConsoleAff where
  map f (ConsoleAff fa) = ConsoleAff $ map f fa

instance applyConsoleAff :: Apply ConsoleAff where
  apply (ConsoleAff ff) (ConsoleAff fa) = ConsoleAff $ apply ff fa

instance applicativeConsoleAff :: Applicative ConsoleAff where
  pure x = ConsoleAff $ pure x

instance bindConsoleAff :: Bind ConsoleAff where
  bind (ConsoleAff fa) f = ConsoleAff $ bind fa (\x -> unwrap $ f x)

instance monadConsoleAff :: Monad ConsoleAff

instance monadAskConsoleAff :: MonadAsk (SPSettings_ SPParams_) ConsoleAff where
  ask = ConsoleAff $ ask

instance monadThrowConsoleAff :: MonadThrow AjaxError ConsoleAff where
  throwError x = ConsoleAff $ throwError x

instance monadErrorConsoleAff :: MonadError AjaxError ConsoleAff where
  catchError (ConsoleAff fa) f = ConsoleAff $ catchError fa (\x -> unwrap $ f x)

instance monadEffectConsoleAff :: MonadEffect ConsoleAff where
  liftEffect x = ConsoleAff $ liftEffect x

instance monadAffConsoleAff :: MonadAff ConsoleAff where
  liftAff = ConsoleAff <<< liftAff

runConsoleAff :: forall a. ConsoleAjaxSettings -> ConsoleAff a -> Aff a
runConsoleAff settings (ConsoleAff fa) = do
  res <- runExceptT $ runReaderT fa settings
  case res of
    Left err -> throwError $ error (Ajax.errorToString err)
    Right v -> pure v