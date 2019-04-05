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

licensePreamble :: String
licensePreamble = "Herd Copyright (C) 2019  A. Alonso Dominguez\n"
               ++ "This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\n"
               ++ "This is free software, and you are welcome to redistribute it\n"
               ++ "under certain conditions; type `show c' for details."

data HerdOpts = HerdOpts
  { _hoConfigFile  :: FilePath
  , _hoInteractive :: Bool
  } deriving (Eq, Show)

makeLenses ''HerdOpts

configFileOpt :: Parser FilePath
configFileOpt = strOption
              ( long "config"
             <> short 'c'
             <> metavar "CONFIG"
             <> value defaultConfigFile
             <> help "Herd configuration file" )

interactiveOpt :: Parser Bool
interactiveOpt = switch
               ( long "interactive"
              <> short 'i'
              <> help "Run the server interactively" )

herdOpts :: Parser HerdOpts
herdOpts = HerdOpts
       <$> configFileOpt
       <*> interactiveOpt

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
herdCli = do
  putStrLn licensePreamble
  herd =<< execParser opts
  where opts = info (herdOpts <**> helper)
             ( fullDesc
            <> progDesc "Event Store Distributed Database"
            <> header "herd" )
