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
    , "effect"
    , "halogen"
    , "psci-support"
    , "servant-support"
    ]
, packages =
    ./packages.dhall
}
