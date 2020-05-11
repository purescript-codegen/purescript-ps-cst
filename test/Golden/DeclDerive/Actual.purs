module Test.Golden.DeclDerive.Actual where

import Language.PS.AST.Sugar
import Language.PS.AST.Types

import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|), singleton)
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (map, pure, ($), (<<<))

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "DeclDerive" :| []
  , imports: []
  , exports: []
  , declarations:
    [ DeclDerive
      { comments: Nothing
      , deriveType: DeclDeriveType_Odrinary
      , head:
        { instName: Ident "eqBaz"
        , instConstraints: []
        , instClass: nonQualifiedName $ ProperName "Eq"
        , instTypes: (TypeConstructor $ nonQualifiedName $ ProperName "Baz") :| []
        }
      }
    , DeclDerive
      { comments: Nothing
      , deriveType: DeclDeriveType_Newtype
      , head:
        { instName: Ident "eqBaz"
        , instConstraints: []
        , instClass: nonQualifiedName $ ProperName "Eq"
        , instTypes: (TypeConstructor $ nonQualifiedName $ ProperName "Baz") :| []
        }
      }
    , DeclDerive
      { comments: Nothing
      , deriveType: DeclDeriveType_Odrinary
      , head:
        { instName: Ident "foo"
        , instConstraints: []
        , instClass: nonQualifiedName $ ProperName "Foo"
        , instTypes: (TypeConstructor $ nonQualifiedName $ ProperName "Bar") :| [TypeRecord $ Row { rowLabels: mkRowLabels [ "foo" /\ numberType ], rowTail: Nothing }]
        }
      }
    , DeclDerive
      { comments: Nothing
      , deriveType: DeclDeriveType_Odrinary
      , head:
        { instName: Ident "foo"
        , instConstraints:
          [ Constraint { className: nonQualifiedName (ProperName "Foo"), args: [typeVar "a"] }
          ]
        , instClass: nonQualifiedName $ ProperName "Foo"
        , instTypes: singleton $ (TypeConstructor $ nonQualifiedName $ ProperName "Array") `TypeApp` (typeVar "a")
        }
      }
    , DeclDerive
      { comments: Nothing
      , deriveType: DeclDeriveType_Odrinary
      , head:
        { instName: Ident "foo"
        , instConstraints:
          [ Constraint { className: nonQualifiedName (ProperName "Foo"), args: [typeVar "a"] }
          , Constraint { className: nonQualifiedName (ProperName "Bar"), args: [typeVar "b", typeVar "c"] }
          , Constraint { className: nonQualifiedName (ProperName "Partial"), args: [] }
          ]
        , instClass: nonQualifiedName $ ProperName "Foo"
        , instTypes: singleton $ (TypeConstructor $ nonQualifiedName $ ProperName "Tuple") `TypeApp` (typeVar "a") `TypeApp` (typeVar "b")
        }
      }
    ]
  }
