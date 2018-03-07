module Herd.CLI
     ( herdCli
     ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Herd.Node

data HerdOpts = HerdOpts
  { host :: String
  , port :: Int
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
       <> help "Port to bind the server to" )

herdOpts :: Parser HerdOpts
herdOpts = HerdOpts
       <$> hostOpt
       <*> portOpt

herd :: HerdOpts -> IO ()
herd (HerdOpts h p) = startHerd h p

herdCli :: IO ()
herdCli = herd =<< execParser opts
  where opts = info (herdOpts <**> helper)
             ( fullDesc
            <> progDesc "Event Store Distributed Database"
            <> header "herd" )
