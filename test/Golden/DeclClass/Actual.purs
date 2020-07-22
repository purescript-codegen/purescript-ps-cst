module Test.Golden.DeclClass.Actual where

import Language.PS.CST.Sugar (mkModuleName, nonQualifiedName, typeVar, typeVarName)
import Language.PS.CST.Types.Shared (ClassFundep(..), Comments(..), Constraint(..), Declaration(..), Ident(..), Module(..), ProperName(..), TypeVarBinding(..), (====>>))

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Prelude (($))

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
        , fundeps: [(NonEmpty.singleton (Ident "a")) `FundepDetermines` (NonEmpty.singleton (Ident "b"))]
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
        , fundeps: [(NonEmpty.singleton (Ident "m")) `FundepDetermines` (NonEmpty.singleton (Ident "c"))]
        }
      , methods: []
      }
    ]
  }
