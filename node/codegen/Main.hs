{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens
import qualified Data.Avro.Schema    as Avro
import           Data.List.Split
import           Data.Proxy          (Proxy (Proxy))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Elm.TyRep
import           Options.Applicative
import           Servant.Elm         (DefineElm (DefineElm),
                                      ElmOptions (stringElmTypes, urlPrefix),
                                      Proxy (Proxy), UrlPrefix (Static),
                                      defElmImports, defElmOptions,
                                      defaultOptions, deriveElmDef,
                                      generateElmModuleWith, toElmType)

import           Herd.Node
import           Herd.Types

deriveElmDef defaultOptions ''SubjectId
deriveElmDef defaultOptions ''Version

instance IsElmDefinition AvroSchema where
  compileElmDef _ = ETypePrimAlias $ EPrimAlias (ETypeName "AvroSchema" []) (ETyCon $ ETCon "Schema")

typeDefs =
  [ DefineElm (Proxy :: Proxy SubjectId)
  , DefineElm (Proxy :: Proxy Version)
  , DefineElm (Proxy :: Proxy AvroSchema)
  ]

apiType = (Proxy :: Proxy HerdREST)

elmOpts :: ElmOptions
elmOpts = defElmOptions
  { urlPrefix      = Static "http://localhost:8081"
  , stringElmTypes = (toElmType (Proxy :: Proxy SubjectId)):(stringElmTypes defElmOptions)
  }

elmImports :: Text
elmImports = T.unlines
  [ "import Herd.Avro exposing (..)"
  , defElmImports
  ]

-- Main program

data HerdCodegenOpts = HerdCodegenOpts
  { _hcoDestFolder   :: FilePath
  , _hcoModuleHeader :: [String]
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

moduleHeaderOpt :: Parser [String]
moduleHeaderOpt = (splitOn ".") <$> strOption
                ( long "out"
               <> short 'o'
               <> metavar "OUTPUT"
               <> help "Output module" )

herdCodegenOpts :: Parser HerdCodegenOpts
herdCodegenOpts = HerdCodegenOpts <$> destFolderOpt <*> moduleHeaderOpt

main :: IO ()
main = runCodeGen =<< execParser opts
  where opts = info (herdCodegenOpts <**> helper)
           ( fullDesc
          <> progDesc "Herd codegen utility"
          <> header "herd-node-codegen" )

runCodeGen :: HerdCodegenOpts -> IO ()
runCodeGen opts =
  generateElmModuleWith elmOpts (opts ^. hcoModuleHeader) elmImports (opts ^. hcoDestFolder) typeDefs apiType
