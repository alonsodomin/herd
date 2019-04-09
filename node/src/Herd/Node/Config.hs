{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Node.Config where

import           Control.Lens
import           Control.Monad.Logger (LogLevel (..))
import           Data.Aeson
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Typeable
import           GHC.Generics

defaultClusterPort :: Int
defaultClusterPort = 9001

defaultHttpPort :: Int
defaultHttpPort = 8081

data NetworkBinding = NetworkBinding
  { _nbHost :: Text
  , _nbPort :: Int
  } deriving (Eq, Show, Generic, Typeable)

makeLenses ''NetworkBinding

instance FromJSON NetworkBinding where
  parseJSON = withObject "network config" $ \o -> do
    _nbHost <- o .: "host"
    _nbPort <- o .: "port"
    return NetworkBinding{..}

data NetworkConfig = NetworkConfig
  { _ncHttp   :: NetworkBinding
  , _ncBroker :: NetworkBinding
  } deriving (Eq, Show, Generic, Typeable)

makeLenses ''NetworkConfig

instance FromJSON NetworkConfig where
  parseJSON = withObject "network config" $ \o -> do
    _ncHttp    <- o .: "http"
    _ncBroker  <- o .: "broker"
    return NetworkConfig{..}

data LoggingDriver =
    LoggingConsole
  | LoggingFile FilePath
  deriving (Eq, Show, Generic, Typeable)

instance FromJSON LoggingDriver where
  parseJSON = withObject "logging driver" $ \o -> do
    driverId <- o .: "driver"
    case (T.toLower driverId) of
      "stdout" -> pure LoggingConsole
      "file"   -> do
        path <- o .: "path"
        return $ LoggingFile path
      _        -> fail $ "Invalid logging driver: " ++ (T.unpack driverId)

defaultLoggingDriver :: LoggingDriver
defaultLoggingDriver = LoggingConsole

data LoggingConfig = LoggingConfig
  { _lcDriver :: LoggingDriver
  , _lcLevel  :: Maybe LogLevel
  } deriving (Eq, Show, Generic, Typeable)

makeLenses ''LoggingConfig

instance FromJSON LoggingConfig where
  parseJSON = withObject "logging config" $ \o -> do
    _lcDriver <- maybe defaultLoggingDriver id <$> o .:? "driver"
    _lcLevel  <- fmap (parseLogLevel . T.toLower) <$> o .:? "level"
    return LoggingConfig{..}

    where parseLogLevel "debug" = LevelDebug
          parseLogLevel "info"  = LevelInfo
          parseLogLevel "warn"  = LevelWarn
          parseLogLevel "error" = LevelError
          parseLogLevel txt     = LevelOther txt

defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig = LoggingConfig defaultLoggingDriver Nothing

data ClusterConfig = ClusterConfig
  { _ccBinding   :: NetworkBinding
  , _ccSeedNodes :: [NetworkBinding]
  } deriving (Eq, Show, Generic, Typeable)

makeLenses ''ClusterConfig

instance FromJSON ClusterConfig where
  parseJSON = withObject "cluster config" $ \o -> do
    _ccBinding   <- o .: "binding"
    _ccSeedNodes <- maybe [] id <$> o .:? "seed-nodes"
    return ClusterConfig{..}

data StorageConfig = StorageConfig
  { _scDataLocation      :: FilePath
  , _scReplicationFactor :: Integer
  } deriving (Eq, Show, Generic, Typeable)

makeLenses ''StorageConfig

instance FromJSON StorageConfig where
  parseJSON = withObject "storage config" $ \o -> do
    _scDataLocation      <- o .: "location"
    _scReplicationFactor <- maybe 1 id <$> o .:? "replication-factor"
    return StorageConfig{..}

data HerdConfig = HerdConfig
  { _hcNetwork :: NetworkConfig
  , _hcLogging :: LoggingConfig
  , _hcCluster :: ClusterConfig
  , _hcStorage :: StorageConfig
  , _hcVersion :: Text
  } deriving (Eq, Show, Generic, Typeable)

makeLenses ''HerdConfig

instance FromJSON HerdConfig where
  parseJSON = withObject "herd config" $ \o -> do
    _hcNetwork <- o .: "network"
    _hcLogging <- maybe defaultLoggingConfig id <$> o .:? "logging"
    _hcCluster <- o .: "cluster"
    _hcStorage <- o .: "storage"
    _hcVersion <- o .: "version"
    return HerdConfig{..}

defaultConfigFile :: FilePath
defaultConfigFile = "./conf/config.yml"
