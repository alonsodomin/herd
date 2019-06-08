{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "herd-console"
, dependencies =
    [ "argonaut-codecs"
    , "bytestrings"
    , "console"
    , "contravariant"
    , "control"
    , "effect"
    , "gen"
    , "halogen"
    , "halogen-mdl"
    , "profunctor-lenses"
    , "psci-support"
    , "quickcheck"
    , "servant-support"
    ]
, packages =
    ./packages.dhall
}
