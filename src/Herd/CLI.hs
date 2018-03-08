module Herd.CLI
     ( herdCli
     ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Herd.Config
import           Herd.Node

data HerdOpts = HerdOpts
  { host       :: String
  , port       :: Int
  , configFile :: FilePath
  } deriving (Eq, Show)

hostOpt :: Parser String
hostOpt = strOption
        ( long "host"
       <> short 'h'
       <> metavar "HOST"
       <> help "Host name to bind to" )

portOpt :: Parser Int
portOpt = option auto
        ( long "port"
       <> short 'p'
       <> metavar "PORT"
       <> value defaultClusterPort
       <> help "Port to bind the server to" )

configFileOpt :: Parser FilePath
configFileOpt = strOption
              ( long "config"
             <> short 'c'
             <> metavar "CONFIG"
             <> value defaultConfigFile
             <> help "Herd configuration file" )

herdOpts :: Parser HerdOpts
herdOpts = HerdOpts
       <$> hostOpt
       <*> portOpt
       <*> configFileOpt

herd :: HerdOpts -> IO ()
herd (HerdOpts h p cfgFile) = startHerd h p cfgFile

herdCli :: IO ()
herdCli = herd =<< execParser opts
  where opts = info (herdOpts <**> helper)
             ( fullDesc
            <> progDesc "Event Store Distributed Database"
            <> header "herd" )
