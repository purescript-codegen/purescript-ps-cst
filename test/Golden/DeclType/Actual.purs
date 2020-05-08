module Test.Golden.DeclType.Actual where

import Language.PS.AST.Sugar
import Language.PS.AST.Types
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested (type (/\), (/\))
import Prelude (map, ($), (<<<))

head =
  DataHead
  { dataHdName: ProperName "Foo"
  , dataHdVars: []
  }

dataMapMap x y =
  (TypeConstructor $ qualifiedName (mkModuleName $ "Data" :| ["Map"]) (ProperName "Map"))
  `TypeApp`
  x
  `TypeApp`
  y

myExtension = nonQualifiedNameTypeConstructor "MyExtension"

declFooType type_ = DeclType { head, type_ }

actualModule :: Module
actualModule = Module
  { moduleName: mkModuleName $ "DeclType" :| []
  , imports: []
  , exports: []
  , declarations:
    [ declFooType boolean
    , declFooType $
        typeRecord
        [ "foo" /\ number
        , "bar" /\ typeRecord [ "baz" /\ dataMapMap string number ]
        , "qwe" /\ typeRecord
          [ "rty" /\ (dataMapMap (typeRecord [ "asd" /\ number ]) (typeRecord [ "foo" /\ number, "bar" /\ (dataMapMap (dataMapMap (dataMapMap number boolean) (dataMapMap number boolean)) boolean) ]))
          , "uio" /\ (dataMapMap (dataMapMap (dataMapMap number boolean) (dataMapMap number boolean)) boolean)
          ]
        ]
    , declFooType $ TypeVar (Ident "a")
    , declFooType $ array $ TypeVar (Ident "a")
    , declFooType $ array $ typeRecord [ "foo" /\ number ]
    , declFooType $ TypeWildcard
    , declFooType $ TypeHole $ Ident "myhole"
    , declFooType $ TypeString "PsString"
    , declFooType $ TypeRow $ Row { rowLabels: [], rowTail: Nothing }
    , declFooType $ TypeRow $ Row { rowLabels: [], rowTail: Just myExtension }
    , declFooType $ TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ number ], rowTail: Nothing }
    , declFooType $ TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ number ], rowTail: Just myExtension }
    , declFooType $ TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ number, "rowField2" /\ number ], rowTail: Nothing }
    , declFooType $ TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ number, "rowField2" /\ number ], rowTail: Just myExtension }
    , declFooType $ TypeRow $ Row { rowLabels: mkRowLabels [ "rowField" /\ number, "rowField2" /\ number ], rowTail: Just $ TypeOp myExtension (nonQualifiedName $ OpName "+") (nonQualifiedNameTypeConstructor "MyOtherExtension") }
    , declFooType $ TypeRow $ Row
      { rowLabels: mkRowLabels [ "rowField" /\ number, "rowField2" /\ number ]
      , rowTail: Just $ TypeOp myExtension
                                (nonQualifiedName $ OpName "+")
                                ((nonQualifiedNameTypeConstructor "MyOtherExtension")
                                `TypeApp`
                                (typeRecord [ "someField" /\ number ])
                                )
      }
    , declFooType $ TypeRow $ Row
      { rowLabels: mkRowLabels
        [ "rowField" /\ (typeRecord
          [ "foo" /\ number
          , "bar" /\ (dataMapMap (dataMapMap (dataMapMap number boolean) (dataMapMap number boolean)) boolean)
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
      (array $ typeVar "a")
    , declFooType $ (array $ typeVar "a") ====>> (maybe $ typeVar "a")
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
        (Constraint { className: nonQualifiedName $ ProperName "MyClass2", args: [typeRecord $ [ "foo" /\ number ]] })
        (typeVar "f"))
    , declFooType $ TypeKinded
      (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
      ((kindNamed "CustomKind" ====>>> KindRow (kindNamed "Type")) ====>>> (kindNamed "Type"))
    , declFooType $ TypeKinded
      (TypeConstructor $ nonQualifiedName $ ProperName "MyKindedType")
      (kindNamed "CustomKind" ====>>> KindRow (kindNamed "Type") ====>>> (kindNamed "Type"))
    ]
  }

type Foo = forall a . a -> a
