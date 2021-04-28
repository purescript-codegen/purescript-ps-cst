module Test.Golden.DeclNewtype.Actual where

import Language.PS.CST (PSConstraint(..), DataHead(..), Declaration(..), Ident(..), Module(..), OpName(..), ProperName(..), PSType(..), TypeVarBinding(..), arrayType, booleanType, maybeType, mkModuleName, mkRowLabels, nonQualifiedName, numberType, qualifiedName, stringType, typeRecord, (====>>))

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

dataMapMap :: PSType -> PSType -> PSType
dataMapMap x y =
  (TypeConstructor $ qualifiedName (mkModuleName $ NonEmpty.cons' "Data" ["Map"]) (ProperName "Map"))
  `TypeApp`
  x
  `TypeApp`
  y

myExtension :: PSType
myExtension = TypeConstructor $ nonQualifiedName $ ProperName "MyExtension"

declFooNewtype :: PSType -> Declaration
declFooNewtype type_ = DeclNewtype { comments: Nothing, head, name: ProperName "Foo", type_ }

typeRow :: PSType
typeRow = (TypeConstructor $ nonQualifiedName $ ProperName "Row") `TypeApp` (TypeConstructor $ nonQualifiedName $ ProperName "Type")

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ NonEmpty.cons' "DeclNewtype" []
  , imports: []
  , exports: []
  , declarations:
    [ declFooNewtype booleanType
    , declFooNewtype $
        typeRecord
        [ "foo" /\ numberType
        , "bar" /\ typeRecord [ "baz" /\ dataMapMap stringType numberType ]
        , "qwe" /\ typeRecord
          [ "rty" /\ (dataMapMap (typeRecord [ "asd" /\ numberType ]) (typeRecord [ "foo" /\ numberType, "bar" /\ (dataMapMap (dataMapMap (dataMapMap numberType booleanType) (dataMapMap numberType booleanType)) booleanType) ]))
          , "uio" /\ (dataMapMap (dataMapMap (dataMapMap numberType booleanType) (dataMapMap numberType booleanType)) booleanType)
          ]
        ]
    , declFooNewtype $ TypeVar (Ident "a")
    , declFooNewtype $ arrayType $ TypeVar (Ident "a")
    , declFooNewtype $ arrayType $ typeRecord [ "foo" /\ numberType ]
    , declFooNewtype $ TypeWildcard
    , declFooNewtype $ TypeHole $ Ident "myhole"
    , declFooNewtype $ TypeString "PsString"
    , declFooNewtype $ TypeRow { rowLabels: [], rowTail: Nothing }
    , declFooNewtype $ TypeRow { rowLabels: [], rowTail: Just myExtension }
    , declFooNewtype $ TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType ], rowTail: Nothing }
    , declFooNewtype $ TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType ], rowTail: Just myExtension }
    , declFooNewtype $ TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Nothing }
    , declFooNewtype $ TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Just myExtension }
    , declFooNewtype $ TypeRow { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Just $ TypeOp myExtension (nonQualifiedName $ OpName "+") (TypeConstructor $ nonQualifiedName $ ProperName "MyOtherExtension") }
    , declFooNewtype $ TypeRow
      { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ]
      , rowTail: Just $ TypeOp myExtension
                                (nonQualifiedName $ OpName "+")
                                ((TypeConstructor $ nonQualifiedName $ ProperName "MyOtherExtension")
                                `TypeApp`
                                (typeRecord [ "someField" /\ numberType ])
                                )
      }
    , declFooNewtype $ TypeRow
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
    , declFooNewtype $ TypeForall
      (NonEmpty.cons' (TypeVarName $ Ident "a") [(TypeVarKinded (Ident "b") (typeRow) )])
      (arrayType $ TypeVar $ Ident "a")
    , declFooNewtype $ (arrayType $ TypeVar $ Ident "a") ====>> (maybeType $ TypeVar $ Ident "a")
    , declFooNewtype $ TypeOp (TypeConstructor $ nonQualifiedName $ ProperName "Array") (nonQualifiedName $ OpName "~>") (TypeConstructor $ nonQualifiedName $ ProperName "Maybe")
    , declFooNewtype $ TypeForall
      (NonEmpty.cons' (TypeVarName $ Ident "f") [])
      ( TypeConstrained
        (PSConstraint { className: nonQualifiedName $ ProperName "Functor", args: [TypeVar $ Ident "f"] })
        (TypeOp (TypeVar $ Ident "f") (nonQualifiedName $ OpName "~>") (TypeConstructor $ nonQualifiedName $ ProperName "Maybe"))
      )
    , declFooNewtype $ TypeConstrained
      (PSConstraint { className: nonQualifiedName $ ProperName "MyClass", args: [TypeVar $ Ident "f", TypeVar $ Ident "g", TypeVar $ Ident "k"] })
      (TypeConstrained
        (PSConstraint { className: nonQualifiedName $ ProperName "MyClass2", args: [typeRecord $ [ "foo" /\ numberType ]] })
        (TypeVar $ Ident "f"))
    , declFooNewtype $ TypeKinded
      (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
      (((TypeConstructor $ nonQualifiedName $ ProperName "CustomKind") ====>> typeRow) ====>> (TypeConstructor $ nonQualifiedName $ ProperName "Type"))
    , declFooNewtype $ TypeKinded
      (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
      ((TypeConstructor $ nonQualifiedName $ ProperName "CustomKind") ====>> typeRow ====>> (TypeConstructor $ nonQualifiedName $ ProperName "Type"))
    ]
  }
