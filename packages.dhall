let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210409/packages.dhall sha256:e81c2f2ce790c0e0d79869d22f7a37d16caeb5bd81cfda71d46c58f6199fd33f

let additions = {=}
-- let additions =
--       { dodo-printer =
--         { dependencies =
--           [ "aff"
--           , "ansi"
--           , "avar"
--           , "console"
--           , "effect"
--           , "foldable-traversable"
--           , "lists"
--           , "maybe"
--           , "minibench"
--           , "node-child-process"
--           , "node-fs-aff"
--           , "node-process"
--           , "psci-support"
--           , "strings"
--           ]
--         , repo = "https://github.com/srghma/purescript-dodo-printer.git"
--         , version = "master"
--         }
--       }

in  upstream // additions
