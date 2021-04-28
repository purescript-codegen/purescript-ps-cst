{ name = "purescript-ps-cst"
, dependencies =
    [ "effect"
    , "psci-support"
    , "strings"
    , "spec"
    , "node-path"
    , "node-fs-aff"
    , "ansi"
    , "dodo-printer"
    , "aff"
    , "arrays"
    , "either"
    , "foldable-traversable"
    , "functions"
    , "lists"
    , "maybe"
    , "newtype"
    , "node-buffer"
    , "ordered-collections"
    , "parallel"
    , "prelude"
    , "transformers"
    , "tuples"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
