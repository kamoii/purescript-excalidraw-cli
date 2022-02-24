{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-excalidraw-cli"
, dependencies =
  [ "aff"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foreign"
  , "maybe"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "optparse"
  , "partial"
  , "prelude"
  , "psci-support"
  , "smolder"
  , "strings"
  , "toppokki"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
