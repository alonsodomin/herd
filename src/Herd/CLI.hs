{-# LANGUAGE TemplateHaskell #-}

module Herd.CLI
     ( herdCli
     ) where

import           Control.Lens
import           Data.Semigroup      ((<>))
import           Data.Yaml           (ParseException, decodeFileEither,
                                      prettyPrintParseException)
import           Options.Applicative

import           Herd.Config
import           Herd.Node

data HerdOpts = HerdOpts
  { _hoConfigFile :: FilePath
  } deriving (Eq, Show)

makeLenses ''HerdOpts

configFileOpt :: Parser FilePath
configFileOpt = strOption
              ( long "config"
             <> short 'c'
             <> metavar "CONFIG"
             <> value defaultConfigFile
             <> help "Herd configuration file" )

herdOpts :: Parser HerdOpts
herdOpts = HerdOpts
       <$> configFileOpt

loadHerdConf :: FilePath -> IO HerdConfig
loadHerdConf configFile = do
  decodedConfig <- parseConfig configFile
  case decodedConfig of
    Left  err -> fail $ prettyPrintParseException err
    Right cfg -> return cfg

  where
    parseConfig :: FilePath -> IO (Either ParseException HerdConfig)
    parseConfig = decodeFileEither

herd :: HerdOpts -> IO ()
herd opts = do
  config <- loadHerdConf (opts ^. hoConfigFile)
  startHerdNode config

herdCli :: IO ()
herdCli = herd =<< execParser opts
  where opts = info (herdOpts <**> helper)
             ( fullDesc
            <> progDesc "Event Store Distributed Database"
            <> header "herd" )
