{-# LANGUAGE TemplateHaskell #-}

module Herd.CLI
     ( herdCli
     ) where

import           Control.Lens
import           Data.Semigroup      ((<>))
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

herd :: HerdOpts -> IO ()
herd opts = startHerd $ opts ^. hoConfigFile

herdCli :: IO ()
herdCli = herd =<< execParser opts
  where opts = info (herdOpts <**> helper)
             ( fullDesc
            <> progDesc "Event Store Distributed Database"
            <> header "herd" )
