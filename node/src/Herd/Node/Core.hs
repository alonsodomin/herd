{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Herd.Node.Core
     ( Logger
     , HerdNode
     , mkHerdNode
     , hnLocalNode
     , hnSchemaRegistry
     , hnSubjectLog
     , HerdEnv
     , mkHerdEnv
     , heLogger
     , heNode
     , heConfig
     , HerdActionT
     , HerdProcess
     , liftAction
     , runAction
     , invokeAction
     --
     , withRegistry
     , withSubjectLog
     ) where

import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TMVar
import           Control.Distributed.Process      (Process)
import           Control.Distributed.Process.Node (LocalNode, runProcess)
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Typeable

import           Herd.Node.Config
import           Herd.Process.SchemaRegistry      (SchemaRegistryServer)
import           Herd.Process.SubjectLog          (SubjectLogServer)

type Logger = LoggingT IO () -> IO ()

data HerdNode = HerdNode
  { _hnLocalNode      :: LocalNode
  , _hnSchemaRegistry :: SchemaRegistryServer
  , _hnSubjectLog     :: SubjectLogServer
  } deriving Typeable

makeLenses ''HerdNode

mkHerdNode :: LocalNode -> SchemaRegistryServer -> SubjectLogServer -> HerdNode
mkHerdNode = HerdNode

data HerdEnv = HerdEnv
  { _heLogger :: Logger
  , _heNode   :: HerdNode
  , _heConfig :: HerdConfig
  }

makeLenses ''HerdEnv

mkHerdEnv :: HerdConfig -> HerdNode -> HerdEnv
mkHerdEnv config node =
  HerdEnv {
    _heLogger = logger (config ^. hcLogging . lcDriver)
  , _heNode   = node
  , _heConfig = config
  }

  where logger :: LoggingDriver -> Logger
        logger LoggingConsole     = runStdoutLoggingT
        logger (LoggingFile path) = runFileLoggingT path

newtype HerdActionT m a = HerdActionT
  { unNodeActionT :: ReaderT HerdEnv m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HerdEnv, MonadTrans, MonadLogger)

type HerdProcess = HerdActionT Process

runAction :: HerdEnv -> HerdActionT m a -> m a
runAction env act = runReaderT (unNodeActionT act) env

liftAction :: Monad m => Getter HerdEnv a -> (a -> m b) -> HerdActionT m b
liftAction getter f = HerdActionT . withReaderT (view getter) $ do
  a <- ask
  lift $ f a

invokeAction :: MonadIO m => HerdProcess a -> HerdActionT m a
invokeAction action = HerdActionT $ do
  env <- ask
  liftIO $ do
    tvar <- newEmptyTMVarIO
    let herdNode = (env ^. heNode)
    runProcess (herdNode ^. hnLocalNode) $ do
      result <- runAction env action
      liftIO $ atomically $ putTMVar tvar result
    atomically $ readTMVar tvar

-- Lifted versions for each process

withRegistry :: Monad m => (SchemaRegistryServer -> m a) -> HerdActionT m a
withRegistry   = liftAction $ heNode . hnSchemaRegistry

withSubjectLog :: Monad m => (SubjectLogServer -> m a) -> HerdActionT m a
withSubjectLog = liftAction $ heNode . hnSubjectLog
