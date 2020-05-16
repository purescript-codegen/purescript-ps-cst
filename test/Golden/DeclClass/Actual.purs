module Test.Golden.DeclClass.Actual where

import Language.PS.CST.Sugar (mkModuleName, nonQualifiedName, typeVar, typeVarName)
import Language.PS.CST.Types (ClassFundep(..), Comments(..), Constraint(..), Declaration(..), Ident(..), Module(..), ProperName(..), TypeVarBinding(..), (====>>))

import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|), singleton)
import Prelude (($))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "DeclClass" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclClass
      { comments: Nothing
      , head:
        { name: ProperName "Foo"
        , vars: [TypeVarName (Ident "a")]
        , super: []
        , fundeps: []
        }
      , methods: []
      }
    , DeclClass
      { comments: Nothing
      , head:
        { name: ProperName "Bar"
        , vars: [TypeVarName (Ident "a")]
        , super: []
        , fundeps: []
        }
      , methods:
        [ { ident: Ident "bar"
          , type_: typeVar "a" ====>> typeVar "a" ====>> typeVar "a"
          }
        ]
      }
    , DeclClass
      { comments: Nothing
      , head:
        { name: ProperName "FunDep"
        , vars: [typeVarName "a", typeVarName "b"]
        , super: []
        , fundeps: [(singleton (Ident "a")) `FundepDetermines` (singleton (Ident "b"))]
        }
      , methods: []
      }
    , DeclClass
      { comments: Nothing
      , head:
        { name: ProperName "MultiFunDep"
        , vars: [typeVarName "a", typeVarName "b", typeVarName "c", typeVarName "d", typeVarName "e"]
        , super: []
        , fundeps:
          [ ((Ident "b" :| [(Ident "c")])) `FundepDetermines` (singleton (Ident "d"))
          , ((Ident "d" :| [])) `FundepDetermines` ((Ident "b" :| [(Ident "c")]))
          ]
        }
      , methods: []
      }
    , DeclClass
      { comments: Nothing
      , head:
        { name: ProperName "Foo"
        , vars: [typeVarName "m"]
        , super:
          [ Constraint { className: nonQualifiedName (ProperName "Bar"), args: [typeVar "m"] }
          ]
        , fundeps: []
        }
      , methods: []
      }
    , DeclClass
      { comments: Just $ OneLineComments ["line1", "line2"]
      , head:
        { name: ProperName "Foo"
        , vars: [typeVarName "m"]
        , super:
          [ Constraint { className: nonQualifiedName (ProperName "Bar"), args: [typeVar "m"] }
          , Constraint { className: nonQualifiedName (ProperName "Baz"), args: [typeVar "m"] }
          ]
        , fundeps: []
        }
      , methods: []
      }
    , DeclClass
      { comments: Just $ BlockComments ["line1", "line2"]
      , head:
        { name: ProperName "Foo"
        , vars: [typeVarName "m", typeVarName "c"]
        , super:
          [ Constraint { className: nonQualifiedName (ProperName "Bar"), args: [typeVar "m", typeVar "c"] }
          ]
        , fundeps: [(singleton (Ident "m")) `FundepDetermines` (singleton (Ident "c"))]
        }
      , methods: []
      }
    ]
  }
