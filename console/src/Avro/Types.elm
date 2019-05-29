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
    | Union { options : Nonempty Type }
