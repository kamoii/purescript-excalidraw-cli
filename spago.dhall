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
  , "foreign"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "optparse"
  , "partial"
  , "prelude"
  , "psci-support"
  , "smolder"
  , "toppokki"
  , "exceptions"
  , "maybe"
  , "newtype"
  , "node-process"
  , "either"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
