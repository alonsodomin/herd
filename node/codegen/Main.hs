{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Avro.Schema as Avro
import           Data.Proxy       (Proxy (Proxy))
import           Elm              (ElmConstructor (..), ElmDatatype (..),
                                   ElmPrimitive (..), ElmType (..),
                                   ElmValue (..), Spec (Spec), specsToDir,
                                   toElmDecoderSource, toElmEncoderSource,
                                   toElmTypeSource)
import           Servant.Elm      (ElmOptions (..), UrlPrefix (Static),
                                   defElmImports, defElmOptions,
                                   generateElmForAPIWith)

import           Herd.Node
import           Herd.Types

instance ElmType Integer where
  toElmType _ = ElmPrimitive EInt

instance ElmType Avro.Type where
  toElmType Avro.Null      = ElmPrimitive EUnit -- FIXME
  toElmType Avro.Boolean   = ElmPrimitive EBool
  toElmType Avro.Int       = ElmPrimitive EInt
  toElmType Avro.Long      = ElmPrimitive EInt
  toElmType Avro.Float     = ElmPrimitive EFloat
  toElmType Avro.Double    = ElmPrimitive EFloat
  toElmType Avro.String    = ElmPrimitive EString
  --toElmType Avro.Bytes     = ElmPrimitive . EList (ElmPrimitive EChar)
  toElmType (Avro.Array x) = ElmPrimitive . EList $ toElmType x
  toElmType (Avro.Map x)   = ElmPrimitive $ EDict EString (toElmType x)

  toElmType t@(Avro.Record name _ _ _ _ fields) =
    let recName          = Avro.typeName t
        recFields []     = ElmEmpty
        recFields (x:xs) = ElmField (Avro.fldName x) $ recFields xs
    in ElmDatatype recName (RecordConstructor recName $ recFields fields)

  toElmType _ = ElmPrimitive EUnit -- FIXME

deriving instance ElmType SubjectId
deriving instance ElmType Version

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "http://localhost:8000/api" }

specs :: [Spec]
specs =
  [ Spec ["Herd", "Console", "Api"]
         (defElmImports
          : toElmTypeSource    (Proxy :: Proxy SubjectId)
          : toElmDecoderSource (Proxy :: Proxy SubjectId)
          : toElmEncoderSource (Proxy :: Proxy SubjectId)
          : toElmTypeSource    (Proxy :: Proxy Version)
          : toElmDecoderSource (Proxy :: Proxy Version)
          : toElmEncoderSource (Proxy :: Proxy Version)
          : generateElmForAPIWith elmOpts  (Proxy :: Proxy HerdREST))
  ]

main :: IO ()
main = specsToDir specs "console/gen"
