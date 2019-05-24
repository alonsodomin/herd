{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import qualified Data.Avro.Schema as Avro
import           Data.Proxy       (Proxy (Proxy))
import           Elm.TyRep
import           Servant.Elm      (DefineElm (DefineElm),
                                   ElmOptions (stringElmTypes, urlPrefix),
                                   Proxy (Proxy), UrlPrefix (Static),
                                   defElmImports, defElmOptions, defaultOptions,
                                   deriveElmDef, generateElmModuleWith,
                                   toElmType)

import           Herd.Node
import           Herd.Types

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

moduleHeader = [ "Herd", "Console", "Remote" ]

destFolder = "console/gen"

main :: IO ()
main = generateElmModuleWith elmOpts moduleHeader defElmImports destFolder typeDefs apiType
