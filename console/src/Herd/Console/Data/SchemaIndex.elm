module Herd.Console.Data.SchemaIndex exposing
    ( SchemaIndex
    , countSubjects
    , countVersions
    , empty
    , filter
    , insert
    , isEmpty
    , latest
    , latestVersion
    )

import Dict exposing (Dict)
import Herd.Console.Remote as Remote exposing (SubjectId, Version)
import List.Nonempty as NEL exposing (Nonempty)


type alias SchemaIndex =
    Dict SubjectId (Nonempty Version)


empty : SchemaIndex
empty =
    Dict.empty


insert : SubjectId -> Nonempty Version -> SchemaIndex -> SchemaIndex
insert subjectId versions =
    Dict.insert subjectId versions


isEmpty : SchemaIndex -> Bool
isEmpty =
    Dict.isEmpty


latestVersion : SubjectId -> SchemaIndex -> Maybe Version
latestVersion subjectId idx =
    Dict.get subjectId idx |> Maybe.map maxFromNEL


latest : Nonempty Version -> Version
latest =
    maxFromNEL


filter : (SubjectId -> Bool) -> SchemaIndex -> SchemaIndex
filter pred =
    Dict.filter (\s _ -> pred s)


countSubjects : SchemaIndex -> Int
countSubjects =
    Dict.size


countVersions : SubjectId -> SchemaIndex -> Int
countVersions subjectId index =
    Dict.get subjectId index
        |> Maybe.map NEL.length
        |> Maybe.withDefault 0



-- Non-empty list helper functions


maxFromNEL : Nonempty comparable -> comparable
maxFromNEL =
    NEL.foldl1
        (\x y ->
            if x < y then
                y

            else
                x
        )
