module Test.Golden.DeclClass.Actual where

import Language.PS.CST

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Prelude

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "DeclClass" []
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
          , type_: (TypeVar $ Ident "a") ====>> (TypeVar $ Ident "a") ====>> (TypeVar $ Ident "a")
          }
        ]
      }
    , DeclClass
      { comments: Nothing
      , head:
        { name: ProperName "FunDep"
        , vars: [TypeVarName $ Ident "a", TypeVarName $ Ident "b"]
        , super: []
        , fundeps: [(NonEmpty.singleton (Ident "a")) `FundepDetermines` (NonEmpty.singleton (Ident "b"))]
        }
      , methods: []
      }
    , DeclClass
      { comments: Nothing
      , head:
        { name: ProperName "MultiFunDep"
        , vars: [TypeVarName $ Ident "a", TypeVarName $ Ident "b", TypeVarName $ Ident "c", TypeVarName $ Ident "d", TypeVarName $ Ident "e"]
        , super: []
        , fundeps:
          [ ((NonEmpty.cons' (Ident "b") [(Ident "c")])) `FundepDetermines` (NonEmpty.singleton (Ident "d"))
          , ((NonEmpty.cons' (Ident "d") [])) `FundepDetermines` ((NonEmpty.cons' (Ident "b") [(Ident "c")]))
          ]
        }
      , methods: []
      }
    , DeclClass
      { comments: Nothing
      , head:
        { name: ProperName "Foo"
        , vars: [TypeVarName $ Ident "m"]
        , super:
          [ Constraint { className: nonQualifiedName (ProperName "Bar"), args: [TypeVar $ Ident "m"] }
          ]
        , fundeps: []
        }
      , methods: []
      }
    , DeclClass
      { comments: Just $ OneLineComments ["line1", "line2"]
      , head:
        { name: ProperName "Foo"
        , vars: [TypeVarName $ Ident "m"]
        , super:
          [ Constraint { className: nonQualifiedName (ProperName "Bar"), args: [TypeVar $ Ident "m"] }
          , Constraint { className: nonQualifiedName (ProperName "Baz"), args: [TypeVar $ Ident "m"] }
          ]
        , fundeps: []
        }
      , methods: []
      }
    , DeclClass
      { comments: Just $ BlockComments ["line1", "line2"]
      , head:
        { name: ProperName "Foo"
        , vars: [TypeVarName $ Ident "m", TypeVarName $ Ident "c"]
        , super:
          [ Constraint { className: nonQualifiedName (ProperName "Bar"), args: [TypeVar $ Ident "m", TypeVar $ Ident "c"] }
          ]
        , fundeps: [(NonEmpty.singleton (Ident "m")) `FundepDetermines` (NonEmpty.singleton (Ident "c"))]
        }
      , methods: []
      }
    ]
  }
