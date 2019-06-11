# Give your project a name. :)
workspace(name = "Herd")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)

#### Haskell Support ####

# Download `rules_haskell`.
# and make it accessible `@io_tweag_rules_haskell`.
http_archive(
    name = "io_tweag_rules_haskell",
    strip_prefix = "rules_haskell-c5c4e67474e8ec20a747eb9bcad00a21f6745069",
    urls = ["https://github.com/tweag/rules_haskell/archive/c5c4e67474e8ec20a747eb9bcad00a21f6745069.tar.gz"],
)

load(
    "@io_tweag_rules_haskell//haskell:repositories.bzl",
    "haskell_repositories",
)

# `haskell_repositories()` sets up all bazel dependencies
# required by `rules_haskell`.
haskell_repositories()

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_register_ghc_bindists",
)

# Registers a haskell toolchain with a GHC binary
# downloaded from haskell.org.
haskell_register_ghc_bindists(version = "8.6.4")

#### PureScript Support ####
