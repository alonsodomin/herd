{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
     , HerdActionT
     , runAction
     , invokeAction
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
  }

makeLenses ''HerdEnv

mkHerdEnv :: HerdConfig -> HerdNode -> HerdEnv
mkHerdEnv config node =
  HerdEnv {
    _heLogger = logger (config ^. hcLogging . lcDriver)
  , _heNode   = node
  }

  where logger :: LoggingDriver -> Logger
        logger LoggingConsole     = runStdoutLoggingT
        logger (LoggingFile path) = runFileLoggingT path

newtype HerdActionT m a = HerdActionT
  { unNodeActionT :: ReaderT HerdEnv m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HerdEnv, MonadTrans, MonadLogger)

runAction :: HerdEnv -> HerdActionT m a -> m a
runAction env act = runReaderT (unNodeActionT act) env

invokeAction :: MonadIO m => (HerdNode -> Process a) -> HerdActionT m a
invokeAction action = HerdActionT $ do
  env <- ask
  liftIO $ do
    tvar <- newEmptyTMVarIO
    let herdNode = (env ^. heNode)
    runProcess (herdNode ^. hnLocalNode) $ do
      result <- action herdNode
      liftIO $ atomically $ putTMVar tvar result
    atomically $ readTMVar tvar
