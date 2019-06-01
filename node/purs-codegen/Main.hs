{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Control.Lens
import           Data.Proxy
import Data.Text (Text)
import           Data.List.Split (splitOn)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
--import           Servant.API
import           Servant.PureScript
import           Options.Applicative

import Herd.Node
import           Herd.Types

-- API Definition

herdAPITypes :: [SumType 'Haskell]
herdAPITypes = [
    mkSumType (Proxy :: Proxy SubjectId)
  , mkSumType (Proxy :: Proxy Version)
  , mkSumType (Proxy :: Proxy AvroSchema)
  ]

-- Command Line Parser

data HerdCodegenOpts = HerdCodegenOpts
  { _hcoDestFolder :: FilePath
  , _hcoModuleName :: Text
  } deriving (Eq, Show)

makeLenses ''HerdCodegenOpts

defaultDestFolder = "console/gen"

destFolderOpt :: Parser FilePath
destFolderOpt = strOption
              ( long "dest"
             <> short 'd'
             <> metavar "DEST_FOLDER"
             <> value defaultDestFolder
             <> help "Destination folder for generated code" )

moduleNameOpt :: Parser Text
moduleNameOpt = strOption
                ( long "out"
               <> short 'o'
               <> metavar "OUTPUT"
               <> help "Output module" )

herdCodegenOpts :: Parser HerdCodegenOpts
herdCodegenOpts = HerdCodegenOpts <$> destFolderOpt <*> moduleNameOpt

main :: IO ()
main = runPursCodegen =<< execParser opts
  where opts = info (herdCodegenOpts <**> helper)
           ( fullDesc
          <> progDesc "Herd PureScript Codegen utility"
          <> header "herd-purs-codegen" )

-- program body

runPursCodegen :: HerdCodegenOpts -> IO ()
runPursCodegen opts = do
  let settings = defaultSettings & apiModuleName .~ (opts ^. hcoModuleName)
  let destFolder = (opts ^. hcoDestFolder)
  writeAPIModuleWithSettings settings destFolder defaultBridgeProxy herdREST
  writePSTypes destFolder (buildBridge defaultBridge) herdAPITypes