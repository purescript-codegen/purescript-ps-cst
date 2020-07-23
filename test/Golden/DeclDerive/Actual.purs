module Test.Golden.DeclDerive.Actual where

import Language.PS.CST

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Data.Tuple.Nested ((/\))
import Prelude (($))

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
          [ Constraint { className: nonQualifiedName (ProperName "Foo"), args: [typeVar "a"] }
          ]
        , instClass: nonQualifiedName $ ProperName "Foo"
        , instTypes: NonEmpty.singleton $
            (TypeConstructor $ nonQualifiedName $ ProperName "Array")
            `TypeApp`
            (typeVar "a")
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
        , instTypes: NonEmpty.singleton $ (TypeConstructor $ nonQualifiedName $ ProperName "Tuple") `TypeApp` (typeVar "a") `TypeApp` (typeVar "b")
        }
      }
    ]
  }
