{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens
import qualified Data.Avro.Schema                           as Avro
import           Data.List.Split                            (splitOn)
import           Data.Proxy
import           Data.Text                                  (Text)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.CodeGenSwitches (ForeignOptions (..),
                                                             genForeign)
import           Language.PureScript.Bridge.PSTypes
import           Options.Applicative
import           Servant.PureScript

import           Herd.Node
import           Herd.Types

-- Type Definitions

haskSubjectId  = Proxy :: Proxy SubjectId
haskVersion    = Proxy :: Proxy Version
haskInteger    = Proxy :: Proxy Integer
haskAvroSchema = Proxy :: Proxy AvroSchema

psAvroType :: PSType
psAvroType = TypeInfo {
    _typePackage = "purescript-avro"
  , _typeModule = "Data.Avro.Types"
  , _typeName = "Type"
  , _typeParameters = []
  }

herdAPITypes :: [SumType 'Haskell]
herdAPITypes = [
    order haskSubjectId $ equal haskSubjectId $ mkSumType haskSubjectId
  , order haskVersion $ equal haskVersion $ mkSumType haskVersion
  ]

-- Language Bridge

avroTypeBridge :: BridgePart
avroTypeBridge = haskType ^== mkTypeInfo haskAvroSchema >> return psAvroType

integerBridge :: BridgePart
integerBridge = haskType ^== mkTypeInfo haskInteger >> return psInt

herdBridge :: BridgePart
herdBridge = defaultBridge <|> integerBridge <|> avroTypeBridge

data HerdBridge

herdBridgeProxy :: Proxy HerdBridge
herdBridgeProxy = Proxy

instance HasBridge HerdBridge where
  languageBridge _ = buildBridge herdBridge

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
  let foreignOpts = ForeignOptions { unwrapSingleConstructors = True }
  let switches = defaultSwitch <> (genForeign foreignOpts)
  let destFolder = (opts ^. hcoDestFolder)
  writeAPIModuleWithSettings settings destFolder herdBridgeProxy herdREST
  writePSTypesWith switches destFolder (buildBridge herdBridge) herdAPITypes
