{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "arrays"
  , "canvas"
  , "console"
  , "lists"
  , "node-fs-aff"
  , "node-path"
  , "psci-support"
  , "random"
  , "refs"
  , "web-dom"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
