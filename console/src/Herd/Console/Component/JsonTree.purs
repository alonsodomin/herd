module Herd.Console.Component.JsonTree where

import Prelude

import Control.Monad.Writer (execWriter, tell)
import Data.Argonaut.Core (Json, caseJson)
import Data.Argonaut.Core as Json
import Data.Array ((:))
import Data.Array as Array
import Data.Either (either)
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.String as Str
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import URL.Validator (parseURL)

type State =
  { value :: Json }

data Query a =
    UpdateJson Json a

type Input = Json

type Message = Void

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver : HE.input UpdateJson
    }

  where initialState :: Json -> State
        initialState json = { value: json }

        render :: State -> H.ComponentHTML Query
        render state =
          HH.pre [ HP.class_ clsJsonDocument ] $ renderJsonRoot state.value        

        eval :: Query ~> H.ComponentDSL State Query Message m
        eval (UpdateJson json next) = do
          H.modify_ (_ { value = json })
          pure next

-- HTML

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

isCollapsible :: Json -> Boolean
isCollapsible json = maybe false (\o -> not $ Object.isEmpty o) $ Json.toObject json

isURL :: String -> Boolean
isURL str = either (const false) (const true) $ parseURL str

escapeString :: String -> String
escapeString str =
  let replacements =
        [ Tuple (Pattern "/&/g") (Replacement "&amp;")
        , Tuple (Pattern "/</g") (Replacement "&lt;")
        , Tuple (Pattern "/>/g") (Replacement "&gt;")
        ]
  in foldl (\s (Tuple p r) -> Str.replace p r s) str replacements

renderToggle :: forall p i. HTML p i
renderToggle = renderToggle' Nothing

renderToggle' :: forall p i. Maybe String -> HTML p i
renderToggle' (Just txt) = HH.a [ HP.class_ clsJsonToggle ] [ HH.text txt ]
renderToggle' Nothing = HH.a [ HP.class_ clsJsonToggle ] []

renderJsonRoot :: forall p i. Json -> Array (HTML p i)
renderJsonRoot json =
  let writeToggle =
        if isCollapsible json then
          tell [ renderToggle ]
          else pure unit
      writeDoc = tell $ renderJson json
  in execWriter $ writeToggle *> writeDoc

renderJson :: forall p i. Json -> Array (HTML p i)
renderJson json =
  caseJson renderNull renderBoolean renderNumber renderString renderArray renderObject json
  where renderNull :: Unit -> Array (HTML p i)
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
          in if isURL str then
               [ HH.a [ HP.href str, HP.class_ clsJsonString, HP.target "_blank" ] [ HH.text escaped ] ]
               else [ HH.span [ HP.class_ clsJsonString ] [ HH.text escaped ] ]

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
                  tell [ renderToggle ]
                  else pure unit
              theItem = tell $ renderJson item
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
                  tell [ renderToggle' $ Just name ]
                  else tell [ HH.text name ]
              theValue  = tell $ (HH.text ": ") : (renderJson value)
              comma =
                if idx < (size - 1) then tell [ HH.text "," ]
                  else pure unit
              fieldBody = fieldName *> theValue *> comma
          in HH.li_ $ execWriter fieldBody
              