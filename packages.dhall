let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200708/packages.dhall sha256:df5b0f1ae92d4401404344f4fb2a7a3089612c9f30066dcddf9eaea4fe780e29

let overrides =
      {
      -- for protolude lib
        either =
              upstream.either
          //  { repo = "https://github.com/srghma/purescript-either.git"
              , version = "patch-1"
              }
      }

let additions =
      { dodo-printer =
        { dependencies =
          [ "aff"
          , "ansi"
          , "avar"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "minibench"
          , "node-child-process"
          , "node-fs-aff"
          , "node-process"
          , "psci-support"
          , "strings"
          ]
        , repo = "https://github.com/srghma/purescript-dodo-printer.git"
        , version = "master"
        }
      , protolude =
        { dependencies =
          [ "affjax"
          , "console"
          , "effect"
          , "node-fs-aff"
          , "node-process"
          , "node-path"
          , "prelude"
          , "proxy"
          , "psci-support"
          , "record"
          , "typelevel-prelude"
          , "debug"
          , "variant"
          , "ansi"
          , "generics-rep"
          ]
        , repo = "https://github.com/srghma/purescript-protolude.git"
        , version = "master"
        }
      }

in  upstream // overrides // additions
