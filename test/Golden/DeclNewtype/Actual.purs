module Test.Golden.DeclNewtype.Actual where

import Language.PS.CST

import Data.Maybe (Maybe(..))
import Data.Array.NonEmpty as NonEmpty
import Data.Tuple.Nested ((/\))
import Prelude (($))

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

declFooNewtype :: Type -> Declaration
declFooNewtype type_ = DeclNewtype { comments: Nothing, head, name: ProperName "Foo", type_ }

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
      (NonEmpty.cons' (TypeVarName $ Ident "a") [(TypeVarKinded (Ident "b") (KindRow (KindName $ nonQualifiedName (ProperName "Type"))) )])
      (arrayType $ typeVar "a")
    , declFooNewtype $ (arrayType $ typeVar "a") ====>> (maybeType $ typeVar "a")
    , declFooNewtype $ TypeOp (TypeConstructor $ nonQualifiedName $ ProperName "Array") (nonQualifiedName $ OpName "~>") (TypeConstructor $ nonQualifiedName $ ProperName "Maybe")
    , declFooNewtype $ TypeForall
      (NonEmpty.cons' (TypeVarName $ Ident "f") [])
      ( TypeConstrained
        (Constraint { className: nonQualifiedName $ ProperName "Functor", args: [typeVar "f"] })
        (TypeOp (typeVar "f") (nonQualifiedName $ OpName "~>") (TypeConstructor $ nonQualifiedName $ ProperName "Maybe"))
      )
    , declFooNewtype $ TypeConstrained
      (Constraint { className: nonQualifiedName $ ProperName "MyClass", args: [typeVar "f", typeVar "g", typeVar "k"] })
      (TypeConstrained
        (Constraint { className: nonQualifiedName $ ProperName "MyClass2", args: [typeRecord $ [ "foo" /\ numberType ]] })
        (typeVar "f"))
    , declFooNewtype $ TypeKinded
      (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
      ((kindNamed "CustomKind" ====>>> KindRow (kindNamed "Type")) ====>>> (kindNamed "Type"))
    , declFooNewtype $ TypeKinded
      (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
      (kindNamed "CustomKind" ====>>> KindRow (kindNamed "Type") ====>>> (kindNamed "Type"))
    ]
  }
