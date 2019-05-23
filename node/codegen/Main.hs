{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Avro.Schema as Avro
import           Data.Proxy       (Proxy (Proxy))
import           Elm
import           Servant.Elm      (ElmOptions (..), UrlPrefix (..),
                                   defElmImports, defElmOptions,
                                   generateElmForAPIWith)

import           Herd.Node
import           Herd.Types

instance ElmType Integer where
  toElmType _ = ElmPrimitive EInt

instance ElmType Avro.Type where
  toElmType _ = ElmDatatype "AvroSchema" (NamedConstructor "AvroSchema" (ElmPrimitiveRef EString))

deriving instance ElmType SubjectId
deriving instance ElmType Version

elmOpts :: ElmOptions
elmOpts =
  defElmOptions { urlPrefix = Static "http://localhost:8081" }

-- embedDec :: Text -> RenderM ()
-- embedDec dec = collectDeclaration (pure $ text dec)

remoteApiSpec :: Spec
remoteApiSpec =
  moduleSpec ["Herd", "Console", "Remote", "Types"] $ do
    -- embedDec "type NoContent = NoContent"
    renderType    (Proxy :: Proxy SubjectId)
    -- renderDecoder (Proxy :: Proxy SubjectId)
    -- renderEncoder (Proxy :: Proxy SubjectId)
    renderType    (Proxy :: Proxy Version)
    -- renderDecoder (Proxy :: Proxy Version)
    -- renderEncoder (Proxy :: Proxy Version)
    renderType    (Proxy :: Proxy Avro.Type)
    -- renderDecoder (Proxy :: Proxy Avro.Type)
    -- renderEncoder (Proxy :: Proxy Avro.Type)

remoteHttpSpec :: Spec
remoteHttpSpec =
  Spec [ "Herd", "Console", "Remote", "HTTP" ]
    ( "import Http"
    : "import Herd.Console.Remote.Types exposing (..)"
    : generateElmForAPIWith elmOpts herdREST
    )

specs :: [Spec]
specs = [remoteApiSpec, remoteHttpSpec]

main :: IO ()
main = specsToDir specs "console/gen"
