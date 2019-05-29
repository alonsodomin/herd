module Herd.Fetch exposing (Fetch(..), view)

import Html exposing (Html, text)


type Fetch a
    = Pending
    | Failed String
    | Ready a


view : (a -> Html msg) -> Fetch a -> Html msg
view render fetch =
    case fetch of
        Pending ->
            text "Pending ..."

        Failed msg ->
            text msg

        Ready data ->
            render data
