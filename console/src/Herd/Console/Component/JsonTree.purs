module Herd.Console.Component.JsonTree
  ( renderJson
  ) where

import Prelude

import Control.Monad.Writer (execWriter, tell)
import Data.Argonaut.Core (Json, caseJson)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (maybe)
import Data.String as Str
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

clsJsonDocument :: HH.ClassName
clsJsonDocument = HH.ClassName "json-document"

clsJsonLiteral :: HH.ClassName
clsJsonLiteral = HH.ClassName "json-literal"

clsJsonString :: HH.ClassName
clsJsonString = HH.ClassName "json-string"

clsJsonArray :: HH.ClassName
clsJsonArray = HH.ClassName "json-array"

clsJsonToggle :: HH.ClassName
clsJsonToggle = HH.ClassName "json-toggle"

clsJsonDict :: HH.ClassName
clsJsonDict = HH.ClassName "json-dict"

escapeString :: String -> String
escapeString str =
  let replacements = [ Tuple (Pattern "/&/g") (Replacement "&amp;") ]
  in foldl (\s (Tuple p r) -> Str.replace p r s) str replacements

renderJson :: forall p i. Json -> HTML p i
renderJson json =
  --HH.div [ HP.class_ clsJsonDocument ]
  HH.span_ $ caseJson renderNull renderBoolean renderNumber renderString renderArray renderObject json
  where isCollapsible :: Json -> Boolean
        isCollapsible j = maybe false (\o -> not $ Object.isEmpty o) $ Json.toObject j
  
        renderNull :: Unit -> Array (HTML p i)
        renderNull _ = [ HH.span [ HP.class_ clsJsonLiteral ] [ HH.text "null" ] ]

        renderNumber :: Number -> Array (HTML p i)
        renderNumber num =
          [ HH.span [ HP.class_ clsJsonLiteral ] [ HH.text $ show num ] ]

        renderBoolean :: Boolean -> Array (HTML p i)
        renderBoolean bool =
          [ HH.span [ HP.class_ clsJsonLiteral ] [ HH.text $ show bool ] ]

        renderString :: String -> Array (HTML p i)
        renderString str =
          let escaped = escapeString str
          in [ HH.span [ HP.class_ clsJsonString ] [ HH.text escaped ] ]

        renderArray :: Array Json -> Array (HTML p i)
        renderArray arr
          | Array.length arr == 0 = [ HH.text "[]" ]
          | otherwise             =
              [ HH.text "["
              , HH.ol
                [ HP.class_ clsJsonArray ]
                $ mapWithIndex (renderArrayItem $ Array.length arr) arr
              , HH.text "]"
              ]

        renderArrayItem :: Int -> Int -> Json -> HTML p i
        renderArrayItem size idx item =
          let toggle =
                if isCollapsible item then
                  tell [ HH.a [ HP.class_ clsJsonToggle ] [] ]
                  else pure unit
              theItem = tell [ renderJson item ]
              comma =
                if idx < (size - 1) then
                  tell [ HH.text "," ]
                  else pure unit
              itemBody = toggle *> theItem *> comma
          in HH.li_ $ execWriter itemBody

        renderObject :: Object Json -> Array (HTML p i)
        renderObject obj
          | Object.isEmpty obj = [ HH.text "{}" ]
          | otherwise          =
              [ HH.text "{"
              , HH.ul
                [ HP.class_ clsJsonDict ] 
                $ mapWithIndex (renderObjectField $ Object.size obj) (Object.toUnfoldable obj)                
              , HH.text "}"
              ]
        
        renderObjectField :: Int -> Int -> Tuple String Json -> HTML p i
        renderObjectField size idx (Tuple name value) =
          let fieldName = 
                if isCollapsible value then
                  tell [ HH.a [ HP.class_ clsJsonToggle ] [ HH.text name ] ]
                  else tell [ HH.text name ]
              theValue  = tell [ HH.text ": ", renderJson value ]
              comma =
                if idx < (size - 1) then tell [ HH.text "," ]
                  else pure unit
              fieldBody = fieldName *> theValue *> comma
          in HH.li_ $ execWriter fieldBody
              