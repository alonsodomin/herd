module Avro.Types exposing (Type(..))

import List.Nonempty as NEL exposing (Nonempty)


type Type
    = Null
    | Boolean
    | Int
    | Union { options : Nonempty Type }
