{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Herd.Config where

import           Control.Lens
import           Data.Aeson
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics

defaultClusterPort :: Int
defaultClusterPort = 9001

defaultHttpPort :: Int
defaultHttpPort = 8081

data NetworkBinding = NetworkBinding
  { _nbHost :: Text
  , _nbPort :: Int
  } deriving (Eq, Show, Generic)

makeLenses ''NetworkBinding

instance FromJSON NetworkBinding where
  parseJSON = withObject "network config" $ \o -> do
    _nbHost <- o .: "host"
    _nbPort <- o .: "port"
    return NetworkBinding{..}

data NetworkConfig = NetworkConfig
  { _ncHttp    :: NetworkBinding
  , _ncBroker  :: NetworkBinding
  } deriving (Eq, Show, Generic)

makeLenses ''NetworkConfig

instance FromJSON NetworkConfig where
  parseJSON = withObject "network config" $ \o -> do
    _ncHttp    <- o .: "http"
    _ncBroker  <- o .: "broker"
    return NetworkConfig{..}

data LoggingDriver =
  LoggingStdOut
  deriving (Eq, Show, Generic, Enum, Ord)

instance FromJSON LoggingDriver where
  parseJSON = withText "logging driver" $ \txt ->
    case (T.toLower txt) of
      "stdout" -> pure LoggingStdOut
      _        -> fail $ "Invalid logging driver: " ++ (T.unpack txt)

defaultLoggingDriver :: LoggingDriver
defaultLoggingDriver = LoggingStdOut

data LoggingConfig = LoggingConfig
  { _lcDriver :: LoggingDriver
  } deriving (Eq, Show, Generic)

makeLenses ''LoggingConfig

instance FromJSON LoggingConfig where
  parseJSON = withObject "logging config" $ \o -> do
    _lcDriver <- maybe defaultLoggingDriver id <$> o .:? "driver"
    return LoggingConfig{..}

defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig = LoggingConfig defaultLoggingDriver

data ClusterConfig = ClusterConfig
  { _ccBinding   :: NetworkBinding
  , _ccSeedNodes :: [NetworkBinding]
  } deriving (Eq, Show, Generic)

makeLenses ''ClusterConfig

instance FromJSON ClusterConfig where
  parseJSON = withObject "cluster config" $ \o -> do
    _ccBinding   <- o .: "binding"
    _ccSeedNodes <- maybe [] id <$> o .:? "seed-nodes"
    return ClusterConfig{..}

data StorageConfig = StorageConfig
  { _scDataLocation :: FilePath
  } deriving (Eq, Show, Generic)

makeLenses ''StorageConfig

instance FromJSON StorageConfig where
  parseJSON = withObject "storage config" $ \o -> do
    _scDataLocation <- o .: "location"
    return StorageConfig{..}

data HerdConfig = HerdConfig
  { _hcNetwork :: NetworkConfig
  , _hcLogging :: LoggingConfig
  , _hcCluster :: ClusterConfig
  , _hcStorage :: StorageConfig
  , _hcVersion :: Text
  } deriving (Eq, Show, Generic)

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
