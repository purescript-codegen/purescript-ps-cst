module Test.Golden.DeclDerive.Actual where

import Language.PS.CST

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Data.Tuple.Nested ((/\))
import Prelude

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "DeclDerive" []
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
        , instTypes: NonEmpty.cons' (TypeConstructor $ nonQualifiedName $ ProperName "Baz") []
        }
      }
    , DeclDerive
      { comments: Nothing
      , deriveType: DeclDeriveType_Newtype
      , head:
        { instName: Ident "eqBaz"
        , instConstraints: []
        , instClass: nonQualifiedName $ ProperName "Eq"
        , instTypes: NonEmpty.cons' (TypeConstructor $ nonQualifiedName $ ProperName "Baz") []
        }
      }
    , DeclDerive
      { comments: Nothing
      , deriveType: DeclDeriveType_Odrinary
      , head:
        { instName: Ident "foo"
        , instConstraints: []
        , instClass: nonQualifiedName $ ProperName "Foo"
        , instTypes: NonEmpty.cons'
          ( TypeConstructor $ nonQualifiedName $ ProperName "Bar")
          [ TypeRecord
            { rowLabels: mkRowLabels [ "foo" /\ numberType ]
            , rowTail: Nothing
            }
          ]
        }
      }
    , DeclDerive
      { comments: Nothing
      , deriveType: DeclDeriveType_Odrinary
      , head:
        { instName: Ident "foo"
        , instConstraints:
          [ Constraint { className: nonQualifiedName (ProperName "Foo"), args: [TypeVar $ Ident "a"] }
          ]
        , instClass: nonQualifiedName $ ProperName "Foo"
        , instTypes: NonEmpty.singleton $
            (TypeConstructor $ nonQualifiedName $ ProperName "Array")
            `TypeApp`
            (TypeVar $ Ident "a")
        }
      }
    , DeclDerive
      { comments: Nothing
      , deriveType: DeclDeriveType_Odrinary
      , head:
        { instName: Ident "foo"
        , instConstraints:
          [ Constraint { className: nonQualifiedName (ProperName "Foo"), args: [TypeVar $ Ident "a"] }
          , Constraint { className: nonQualifiedName (ProperName "Bar"), args: [TypeVar $ Ident "b", TypeVar $ Ident "c"] }
          , Constraint { className: nonQualifiedName (ProperName "Partial"), args: [] }
          ]
        , instClass: nonQualifiedName $ ProperName "Foo"
        , instTypes: NonEmpty.singleton $ (TypeConstructor $ nonQualifiedName $ ProperName "Tuple") `TypeApp` (TypeVar $ Ident "a") `TypeApp` (TypeVar $ Ident "b")
        }
      }
    , DeclDerive
      { comments: Nothing
      , deriveType: DeclDeriveType_Odrinary
      , head:
        { instName: Ident "foo"
        , instConstraints:
          [ Constraint { className: nonQualifiedName (ProperName "Foo"), args: [TypeVar $ Ident "a"] }
          , Constraint { className: nonQualifiedName (ProperName "Bar"), args: [TypeVar $ Ident "b", TypeVar $ Ident "c"] }
          , Constraint { className: nonQualifiedName (ProperName "Partial"), args: [] }
          , Constraint { className: nonQualifiedName (ProperName "Partial1"), args: [] }
          , Constraint { className: nonQualifiedName (ProperName "Partial2"), args: [] }
          ]
        , instClass: nonQualifiedName $ ProperName "Foo"
        , instTypes: NonEmpty.singleton $ (TypeConstructor $ nonQualifiedName $ ProperName "Tuple") `TypeApp` (TypeVar $ Ident "a") `TypeApp` (TypeVar $ Ident "b")
        }
      }
    ]
  }
