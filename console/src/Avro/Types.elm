module Avro.Types exposing (Type(..))

import List.Nonempty as NEL exposing (Nonempty)


type Type
    = Null
    | Boolean
    | Int
    | Long
    | Float
    | Double
    | Bytes
    | String
    | Array { items : Type }
    | Map { values : Type }
    | Union { options : Nonempty Type }
