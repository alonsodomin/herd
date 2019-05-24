{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Avro.Schema as Avro
import           Data.Proxy       (Proxy (Proxy))
-- import           Elm
import Elm.TyRep
import           Servant.Elm  (DefineElm (DefineElm), ElmOptions(urlPrefix, stringElmTypes),
                               Proxy (Proxy), UrlPrefix(Static), defaultOptions,
                               defElmImports, defElmOptions, deriveElmDef,
                               generateElmModuleWith, toElmType)

import           Herd.Node
import           Herd.Types

-- instance ElmType Integer where
--   toElmType _ = ElmPrimitive EInt

-- instance ElmType Avro.Type where
--   toElmType _ = ElmDatatype "AvroSchema" (NamedConstructor "AvroSchema" (ElmPrimitiveRef EString))

-- deriving instance ElmType SubjectId
-- deriving instance ElmType Version

deriveElmDef defaultOptions ''SubjectId
deriveElmDef defaultOptions ''Version
-- deriveElmDef defaultOptions ''AvroSchema

instance IsElmDefinition AvroSchema where
  compileElmDef _ = ETypePrimAlias $ EPrimAlias (ETypeName "AvroSchema" []) (ETyCon $ ETCon "String")

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

-- embedDec :: Text -> RenderM ()
-- embedDec dec = collectDeclaration (pure $ text dec)

-- remoteApiSpec :: Spec
-- remoteApiSpec =
--   moduleSpec ["Herd", "Console", "Remote", "Types"] $ do
--     renderType    (Proxy :: Proxy SubjectId)
--     renderDecoder (Proxy :: Proxy SubjectId)
--     -- renderEncoder (Proxy :: Proxy SubjectId)
--     renderType    (Proxy :: Proxy Version)
--     renderDecoder (Proxy :: Proxy Version)
--     -- renderEncoder (Proxy :: Proxy Version)
--     renderType    (Proxy :: Proxy Avro.Type)
--     renderDecoder (Proxy :: Proxy Avro.Type)
--     -- renderEncoder (Proxy :: Proxy Avro.Type)

-- remoteHttpSpec :: Spec
-- remoteHttpSpec =
--   Spec [ "Herd", "Console", "Remote", "HTTP" ]
--     ( "import Http"
--     : "import Herd.Console.Remote.Types exposing (..)"
--     : generateElmForAPIWith elmOpts herdREST
--     )

-- specs :: [Spec]
-- specs = [remoteHttpSpec]

-- main :: IO ()
-- main = specsToDir specs "console/gen"

moduleHeader = [ "Herd", "Console", "Remote" ]

destFolder = "console/gen"

main :: IO ()
main = generateElmModuleWith elmOpts moduleHeader defElmImports destFolder typeDefs apiType
