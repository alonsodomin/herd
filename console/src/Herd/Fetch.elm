module Herd.Fetch exposing (Fetch, failed, fromResult, map, pending, succeeded, view, withDefault)

import Html exposing (Html, text)
import Http


type Fetch a
    = Pending
    | Ready (Result Http.Error a)


fromResult : Result Http.Error a -> Fetch a
fromResult =
    Ready


pending : Fetch a
pending =
    Pending


succeeded : a -> Fetch a
succeeded x =
    Ready (Ok x)


failed : Http.Error -> Fetch a
failed err =
    Ready (Err err)


map : (a -> b) -> Fetch a -> Fetch b
map f fetched =
    case fetched of
        Pending ->
            Pending

        Ready result ->
            Ready (Result.map f result)


withDefault : a -> Fetch a -> a
withDefault default fetched =
    case fetched of
        Ready (Ok value) ->
            value

        _ ->
            default


view : (a -> Html msg) -> Fetch a -> Html msg
view render fetch =
    case fetch of
        Pending ->
            text "Pending ..."

        Ready (Ok data) ->
            render data

        Ready (Err err) ->
            viewError err


viewError : Http.Error -> Html msg
viewError err =
    case err of
        Http.BadUrl url ->
            text ("Bad url: " ++ url)

        Http.Timeout ->
            text "HTTP request timed out"

        Http.NetworkError ->
            text "HTTP network error"

        Http.BadStatus code ->
            text ("Bad HTTP status code: " ++ String.fromInt code)

        Http.BadBody msg ->
            text ("Bad HTTP body: " ++ msg)
