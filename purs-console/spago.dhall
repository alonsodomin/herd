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
    , "control"
    , "effect"
    , "gen"
    , "halogen"
    , "psci-support"
    , "quickcheck"
    , "servant-support"
    ]
, packages =
    ./packages.dhall
}
