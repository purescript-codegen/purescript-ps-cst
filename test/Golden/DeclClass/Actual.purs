module Test.Golden.DeclClass.Actual where

import Language.PS.AST.Sugar
import Language.PS.AST.Types

import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|), singleton)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (map, pure, ($), (<<<))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "DeclClass" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclClass
      { head:
        { name: ProperName "Foo"
        , vars: [TypeVarName (Ident "a")]
        , super: Nothing
        , fundeps: []
        }
      , methods: []
      }
    , DeclClass
      { head:
        { name: ProperName "Bar"
        , vars: [TypeVarName (Ident "a")]
        , super: Nothing
        , fundeps: []
        }
      , methods:
        [ { ident: Ident "bar"
          , type_: typeVar "a" ====>> typeVar "a" ====>> typeVar "a"
          }
        ]
      }
    , DeclClass
      { head:
        { name: ProperName "FunDep"
        , vars: [typeVarName "a", typeVarName "b"]
        , super: Nothing
        , fundeps: [(singleton (Ident "a")) `FundepDetermines` (singleton (Ident "b"))]
        }
      , methods: []
      }
    , DeclClass
      { head:
        { name: ProperName "MultiFunDep"
        , vars: [typeVarName "a", typeVarName "b", typeVarName "c", typeVarName "d", typeVarName "e"]
        , super: Nothing
        , fundeps:
          [ ((Ident "b" :| [(Ident "c")])) `FundepDetermines` (singleton (Ident "d"))
          , ((Ident "d" :| [])) `FundepDetermines` ((Ident "b" :| [(Ident "c")]))
          ]
        }
      , methods: []
      }
    , DeclClass
      { head:
        { name: ProperName "Foo"
        , vars: [typeVarName "m"]
        , super: Just $ Constraint { className: nonQualifiedName (ProperName "Bar"), args: [typeVar "m"] }
        , fundeps: []
        }
      , methods: []
      }
    , DeclClass
      { head:
        { name: ProperName "Foo"
        , vars: [typeVarName "m", typeVarName "c"]
        , super: Just $ Constraint { className: nonQualifiedName (ProperName "Bar"), args: [typeVar "m", typeVar "c"] }
        , fundeps: [(singleton (Ident "m")) `FundepDetermines` (singleton (Ident "c"))]
        }
      , methods: []
      }
    ]
  }
