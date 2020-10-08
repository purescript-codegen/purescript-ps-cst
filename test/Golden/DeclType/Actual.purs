module Test.Golden.DeclType.Actual where

import Language.PS.CST (Constraint(..), DataHead(..), Declaration(..), Ident(..), Kind(..), Module(..), OpName(..), ProperName(..), Type(..), TypeVarBinding(..), arrayType, booleanType, maybeType, mkModuleName, mkRowLabels, nonQualifiedName, numberType, qualifiedName, stringType, typeRecord, (====>>), (====>>>))

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Data.Tuple.Nested ((/\))
import Prelude

head :: DataHead
head =
  DataHead
  { dataHdName: ProperName "Foo"
  , dataHdVars: []
  }

dataMapMap :: Type -> Type -> Type
dataMapMap x y =
  (TypeConstructor $ qualifiedName (mkModuleName $ NonEmpty.cons' "Data" ["Map"]) (ProperName "Map"))
  `TypeApp`
  x
  `TypeApp`
  y

myExtension :: Type
myExtension = TypeConstructor $ nonQualifiedName $ ProperName "MyExtension"

declFooType :: Type -> Declaration
declFooType type_ = DeclType { comments: Nothing, head, type_ }

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "DeclType" []
  , imports: []
  , exports: []
  , declarations:
    [ declFooType booleanType
    , declFooType $
        typeRecord
        [ "foo" /\ numberType
        , "bar" /\ typeRecord [ "baz" /\ dataMapMap stringType numberType ]
        , "qwe" /\ typeRecord
          [ "rty" /\
            (dataMapMap
              ( typeRecord [ "asd" /\ numberType ] )
              ( typeRecord
                [ "foo" /\ numberType
                , "bar" /\ ( dataMapMap
                             ( dataMapMap
                               (dataMapMap numberType booleanType)
                               (dataMapMap numberType booleanType)
                             )
                             booleanType
                           )
                ]
              )
            )
          , "uio" /\ (dataMapMap (dataMapMap (dataMapMap numberType booleanType) (dataMapMap numberType booleanType)) booleanType)
          ]
        ]
    , declFooType $ TypeVar (Ident "a")
    , declFooType $ arrayType $ TypeVar (Ident "a")
    , declFooType $ arrayType $ typeRecord [ "foo" /\ numberType ]
    , declFooType $ TypeWildcard
    , declFooType $ TypeHole $ Ident "myhole"
    , declFooType $ TypeString "PsString"
    , declFooType $ TypeRow { rowLabels: [], rowTail: Nothing }
    , declFooType $ TypeRow { rowLabels: [], rowTail: Just myExtension }
    , declFooType $ TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType ], rowTail: Nothing }
    , declFooType $ TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType ], rowTail: Just myExtension }
    , declFooType $ TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Nothing }
    , declFooType $ TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Just myExtension }
    , declFooType $ TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Just $ TypeOp myExtension (nonQualifiedName $ OpName "+") (TypeConstructor $ nonQualifiedName $ ProperName "MyOtherExtension") }
    , declFooType $ TypeRow
      { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ]
      , rowTail: Just $ TypeOp myExtension
                                (nonQualifiedName $ OpName "+")
                                ((TypeConstructor $ nonQualifiedName $ ProperName "MyOtherExtension")
                                `TypeApp`
                                (typeRecord [ "someField" /\ numberType ])
                                )
      }
    , declFooType $ TypeRow
      { rowLabels: mkRowLabels [ "RowField" /\ numberType ]
      , rowTail: Nothing
      }
    , declFooType $ TypeRow
      { rowLabels: mkRowLabels
        [ "rowField" /\ (typeRecord
          [ "foo" /\ numberType
          , "bar" /\ (dataMapMap (dataMapMap (dataMapMap numberType booleanType) (dataMapMap numberType booleanType)) booleanType)
          , "baz" /\ (
            (TypeConstructor $ nonQualifiedName (ProperName "Complex"))
            `TypeApp`
            (TypeConstructor $ nonQualifiedName (ProperName "A"))
            `TypeApp`
            (TypeConstructor $ nonQualifiedName (ProperName "B"))
            `TypeApp`
            (TypeConstructor $ nonQualifiedName (ProperName "C"))
            `TypeApp`
            (TypeConstructor $ nonQualifiedName (ProperName "D"))
            `TypeApp`
            (TypeConstructor $ nonQualifiedName (ProperName "F"))
            `TypeApp`
            (TypeConstructor $ nonQualifiedName (ProperName "G"))
            `TypeApp`
            (TypeConstructor $ nonQualifiedName (ProperName "H"))
          )
          , "qux" /\ (
            (TypeConstructor $ nonQualifiedName (ProperName "Complex"))
            `TypeApp`
            (
              (TypeConstructor $ nonQualifiedName (ProperName "A"))
              `TypeApp`
              (TypeConstructor $ nonQualifiedName (ProperName "B"))
              `TypeApp`
              (TypeConstructor $ nonQualifiedName (ProperName "C"))
            )
            `TypeApp`
            (TypeConstructor $ nonQualifiedName (ProperName "D"))
            `TypeApp`
            (
              (TypeConstructor $ nonQualifiedName (ProperName "F"))
              `TypeApp`
              (TypeConstructor $ nonQualifiedName (ProperName "G"))
              `TypeApp`
              (TypeConstructor $ nonQualifiedName (ProperName "H"))
            )
          )
          ])
        ]
      , rowTail: Nothing
      }
    , declFooType $ TypeForall
      ( NonEmpty.cons'
        (TypeVarName $ Ident "a")
        [ (TypeVarKinded (Ident "b") (KindRow (KindName $ nonQualifiedName (ProperName "Type"))) )
        ]
      )
      (arrayType $ TypeVar $ Ident "a")
    , declFooType $ (arrayType $ TypeVar $ Ident "a") ====>> (maybeType $ TypeVar $ Ident "a")
    , declFooType $ TypeOp (TypeConstructor $ nonQualifiedName $ ProperName "Array") (nonQualifiedName $ OpName "~>") (TypeConstructor $ nonQualifiedName $ ProperName "Maybe")
    , declFooType $ TypeForall
      (NonEmpty.cons' (TypeVarName $ Ident "f") [])
      ( TypeConstrained
        (Constraint { className: nonQualifiedName $ ProperName "Functor", args: [TypeVar $ Ident "f"] })
        (TypeOp (TypeVar $ Ident "f") (nonQualifiedName $ OpName "~>") (TypeConstructor $ nonQualifiedName $ ProperName "Maybe"))
      )
    , declFooType $ TypeConstrained
      (Constraint { className: nonQualifiedName $ ProperName "MyClass", args: [TypeVar $ Ident "f", TypeVar $ Ident "g", TypeVar $ Ident "k"] })
      (TypeConstrained
        (Constraint { className: nonQualifiedName $ ProperName "MyClass2", args: [typeRecord $ [ "foo" /\ numberType ]] })
        (TypeVar $ Ident "f"))
    , declFooType $ TypeKinded
      (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
      (((KindName $ nonQualifiedName $ ProperName "CustomKind") ====>>> KindRow (KindName $ nonQualifiedName $ ProperName "Type")) ====>>> (KindName $ nonQualifiedName $ ProperName "Type"))
    , declFooType $ TypeKinded
      (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
      ((KindName $ nonQualifiedName $ ProperName "CustomKind") ====>>> KindRow (KindName $ nonQualifiedName $ ProperName "Type") ====>>> (KindName $ nonQualifiedName $ ProperName "Type"))
    ]
  }
