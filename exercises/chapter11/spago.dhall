{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "node-readline"
    , "ordered-collections"
    , "psci-support"
    , "strings"
    , "transformers"
    , "test-unit"
    , "yargs"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
