module Test.Golden.DeclType.Actual where

import Language.PS.AST.Sugar (arrayType, booleanType, kindNamed, maybeType, mkModuleName, mkRowLabels, nonQualifiedName, nonQualifiedNameTypeConstructor, numberType, qualifiedName, stringType, typeRecord, typeVar, typeVarName)
import Language.PS.AST.Types (Constraint(..), DataHead(..), Declaration(..), Ident(..), Kind(..), Module(..), OpName(..), ProperName(..), Row(..), Type(..), TypeVarBinding(..), (====>>), (====>>>))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
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
  (TypeConstructor $ qualifiedName (mkModuleName $ "Data" :| ["Map"]) (ProperName "Map"))
  `TypeApp`
  x
  `TypeApp`
  y

myExtension :: Type
myExtension = nonQualifiedNameTypeConstructor "MyExtension"

declFooType :: Type -> Declaration
declFooType type_ = DeclType { comments: Nothing, head, type_ }

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "DeclType" :| []
  , imports: []
  , exports: []
  , declarations:
    [ declFooType booleanType
    , declFooType $
        typeRecord
        [ "foo" /\ numberType
        , "bar" /\ typeRecord [ "baz" /\ dataMapMap stringType numberType ]
        , "qwe" /\ typeRecord
          [ "rty" /\ (dataMapMap (typeRecord [ "asd" /\ numberType ]) (typeRecord [ "foo" /\ numberType, "bar" /\ (dataMapMap (dataMapMap (dataMapMap numberType booleanType) (dataMapMap numberType booleanType)) booleanType) ]))
          , "uio" /\ (dataMapMap (dataMapMap (dataMapMap numberType booleanType) (dataMapMap numberType booleanType)) booleanType)
          ]
        ]
    , declFooType $ TypeVar (Ident "a")
    , declFooType $ arrayType $ TypeVar (Ident "a")
    , declFooType $ arrayType $ typeRecord [ "foo" /\ numberType ]
    , declFooType $ TypeWildcard
    , declFooType $ TypeHole $ Ident "myhole"
    , declFooType $ TypeString "PsString"
    , declFooType $ TypeRow $ Row { rowLabels: [], rowTail: Nothing }
    , declFooType $ TypeRow $ Row { rowLabels: [], rowTail: Just myExtension }
    , declFooType $ TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ numberType ], rowTail: Nothing }
    , declFooType $ TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ numberType ], rowTail: Just myExtension }
    , declFooType $ TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Nothing }
    , declFooType $ TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Just myExtension }
    , declFooType $ TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ], rowTail: Just $ TypeOp myExtension (nonQualifiedName $ OpName "+") (nonQualifiedNameTypeConstructor "MyOtherExtension") }
    , declFooType $ TypeRow $ Row
      { rowLabels: mkRowLabels [ "rowField" /\ numberType, "rowField2" /\ numberType ]
      , rowTail: Just $ TypeOp myExtension
                                (nonQualifiedName $ OpName "+")
                                ((nonQualifiedNameTypeConstructor "MyOtherExtension")
                                `TypeApp`
                                (typeRecord [ "someField" /\ numberType ])
                                )
      }
    , declFooType $ TypeRow $ Row
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
      ((typeVarName "a") :| [(TypeVarKinded (Ident "b") (KindRow (KindName $ nonQualifiedName (ProperName "Type"))) )])
      (arrayType $ typeVar "a")
    , declFooType $ (arrayType $ typeVar "a") ====>> (maybeType $ typeVar "a")
    , declFooType $ TypeOp (nonQualifiedNameTypeConstructor "Array") (nonQualifiedName $ OpName "~>") (nonQualifiedNameTypeConstructor "Maybe")
    , declFooType $ TypeForall
      ((typeVarName "f") :| [])
      ( TypeConstrained
        (Constraint { className: nonQualifiedName $ ProperName "Functor", args: [typeVar "f"] })
        (TypeOp (typeVar "f") (nonQualifiedName $ OpName "~>") (nonQualifiedNameTypeConstructor "Maybe"))
      )
    , declFooType $ TypeConstrained
      (Constraint { className: nonQualifiedName $ ProperName "MyClass", args: [typeVar "f", typeVar "g", typeVar "k"] })
      (TypeConstrained
        (Constraint { className: nonQualifiedName $ ProperName "MyClass2", args: [typeRecord $ [ "foo" /\ numberType ]] })
        (typeVar "f"))
    , declFooType $ TypeKinded
      (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
      ((kindNamed "CustomKind" ====>>> KindRow (kindNamed "Type")) ====>>> (kindNamed "Type"))
    , declFooType $ TypeKinded
      (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
      (kindNamed "CustomKind" ====>>> KindRow (kindNamed "Type") ====>>> (kindNamed "Type"))
    ]
  }
