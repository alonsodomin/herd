module Herd.Console.Data.SchemaIndex exposing (SchemaIndex, empty, latestVersion)

import Dict exposing (Dict)
import Herd.Console.Remote as Remote exposing (SubjectId, Version)


type alias SchemaIndex =
    Dict SubjectId (List Version)


empty : SchemaIndex
empty =
    Dict.empty


latestVersion : SubjectId -> SchemaIndex -> Maybe Version
latestVersion subjectId idx =
    Dict.get subjectId idx |> Maybe.andThen List.maximum
