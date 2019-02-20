VERION = "0.8"

http_archive(
  name = "io_tweag_rules_haskell",
  strip_prefix = "rules_haskell-$VERSION",
  urls = ["https://github.com/tweag/rules_haskell/archive/v$VERSION.tar.gz"],
)

load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
	"haskell_repositories",
	"haskell_register_toolchains",
)

haskell_repositories()

haskell_register_toolchains()