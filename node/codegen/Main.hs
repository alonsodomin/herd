{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Avro.Schema as Avro
import           Data.Proxy       (Proxy (Proxy))
import           Elm              
import           Servant.Elm      (ElmOptions (..), UrlPrefix (Static),
                                   defElmImports, defElmOptions,
                                   generateElmForAPIWith)
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

import           Herd.Node
import           Herd.Types

instance ElmType Integer where
  toElmType _ = ElmPrimitive EInt

instance ElmType Avro.Type where
  toElmType _ = ElmDatatype "AvroSchema" (NamedConstructor "AvroSchema" (ElmPrimitiveRef EString))

deriving instance ElmType SubjectId
deriving instance ElmType Version

-- instance HasEncoder SubjectId where
--   render (SubjectId x) = pure $ "Json.Encode.string" <+> (render x)

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "http://localhost:8081" }

specs :: [Spec]
specs =
  [ Spec ["Herd", "Console", "Api"]
         (defElmImports
          : "type NoContent = NoContent"
          : toElmTypeSource    (Proxy :: Proxy SubjectId)
          : toElmDecoderSource (Proxy :: Proxy SubjectId)
          -- : toElmEncoderSource (Proxy :: Proxy SubjectId)
          : toElmTypeSource    (Proxy :: Proxy Version)
          : toElmDecoderSource (Proxy :: Proxy Version)
          -- : toElmEncoderSource (Proxy :: Proxy Version)
          : toElmTypeSource    (Proxy :: Proxy Avro.Type)
          : toElmDecoderSource (Proxy :: Proxy Avro.Type)
          -- : toElmEncoderSource (Proxy :: Proxy Avro.Type)
          : generateElmForAPIWith elmOpts herdREST
          )
  ]

main :: IO ()
main = specsToDir specs "console/gen"
