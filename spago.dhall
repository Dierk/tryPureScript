{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "assert"
  , "console"
  , "control"
  , "effect"
  , "lists"
  , "psci-support"
  , "quickcheck"
  , "strings"
  , "stringutils"
  , "transformers"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "experimental/src/**/*.purs", "experimental/test/**/*.purs" ]
}
