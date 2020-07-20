{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "datetime"
  , "effect"
  , "enums"
  , "folds"
  , "integers"
  , "js-date"
  , "lists"
  , "maybe"
  , "now"
  , "numbers"
  , "psci-support"
  , "spec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
