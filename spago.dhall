{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-excalidraw-cli"
, dependencies =
  [ "aff"
  , "console"
  , "control"
  , "crypto"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "node-process"
  , "optparse"
  , "partial"
  , "prelude"
  , "psci-support"
  , "smolder"
  , "toppokki"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
