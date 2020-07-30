{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "teller"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "enums"
  , "foldable-traversable"
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
