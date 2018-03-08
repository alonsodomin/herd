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

data StorageConfig = StorageConfig
  { _scDataLocation :: FilePath
  } deriving (Eq, Show, Generic)

makeLenses ''StorageConfig

instance FromJSON StorageConfig where
  parseJSON = withObject "storage config" $ \o -> do
    _scDataLocation <- o .: "location"
    return StorageConfig{..}

data HerdConfig = HerdConfig
  { _hcLogging :: LoggingConfig
  , _hcStorage :: StorageConfig
  , _hcVersion :: Text
  } deriving (Eq, Show, Generic)

makeLenses ''HerdConfig

instance FromJSON HerdConfig where
  parseJSON = withObject "herd config" $ \o -> do
    _hcLogging <- maybe defaultLoggingConfig id <$> o .:? "logging"
    _hcStorage <- o .: "storage"
    _hcVersion <- o .: "version"
    return HerdConfig{..}

defaultConfigFile :: FilePath
defaultConfigFile = "./conf/config.yml"
