{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let override =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { "package-name" =
       mkPackage
         [ "dependency1"
         , "dependency2"
         ]
         "https://example.com/path/to/git/repo.git"
         "tag ('v4.0.0') or branch ('master')"
  , "package-name" =
       mkPackage
         [ "dependency1"
         , "dependency2"
         ]
         "https://example.com/path/to/git/repo.git"
         "tag ('v4.0.0') or branch ('master')"
  , etc.
  }
-------------------------------

Example:
-------------------------------
let additions =
  { benchotron =
      mkPackage
        [ "arrays"
        , "exists"
        , "profunctor"
        , "strings"
        , "quickcheck"
        , "lcg"
        , "transformers"
        , "foldable-traversable"
        , "exceptions"
        , "node-fs"
        , "node-buffer"
        , "node-readline"
        , "datetime"
        , "now"
        ]
        "https://github.com/hdgarrood/purescript-benchotron.git"
        "v7.0.0"
  }
-------------------------------
-}

let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190525/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.5-20190525/src/packages.dhall sha256:d52b72daa09ca9eca2d62744ea051177773cfaec4303cb23b4bc1b156344eed5

let overrides = {=}

let additions =
  { servant-support =
      mkPackage
        [ "prelude"
        , "console"
        , "either"
        , "foldable-traversable"
        , "generics-rep"
        , "effect"
        , "aff"
        , "exceptions"
        , "web-xhr"
        , "foreign-generic"
        , "affjax"
        ]
        "https://github.com/shmish111/purescript-servant-support.git"
        "226fdec428a4fcd8ef57258c50babcf60b808d30"
  , quotient =
      mkPackage
        [ "proxy"
        , "prelude"
        , "quickcheck"
        ]
        "https://github.com/rightfold/purescript-quotient.git"
        "v3.0.0"
  , bytestrings =
      mkPackage
        [ "exceptions"
        , "newtype"
        , "effect"
        , "arrays"
        , "maybe"
        , "quotient"
        , "foldable-traversable"
        , "leibniz"
        , "prelude"
        , "unsafe-coerce"
        , "integers"
        , "node-buffer"
        , "quickcheck"
        ]
        "https://github.com/rightfold/purescript-bytestrings.git"
        "v7.0.0"
  , halogen-mdl =
      mkPackage
        [ "console"
        , "effect"
        , "halogen"
        , "halogen-css"
        , "numbers"
        , "psci-support"
        , "web-dom"
        , "web-html"
        , "web-uievents"
        ]
        "https://github.com/alonsodomin/purescript-halogen-mdl.git"
        "94a6464057e03c18b5b9a11f264d0d2ab3ee72ab"
  , url-validator =
      mkPackage
        [ "nullable" ]
        "https://github.com/bbarker/purescript-url-validator.git"
        "v1.1.0"
  }

in  upstream // overrides // additions
